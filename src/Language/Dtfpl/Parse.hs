{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- | Parser for dtfpl.
module Language.Dtfpl.Parse
    ( parse
    ) where

import           Control.Category                   ((>>>))
import           Control.Monad.Combinators.NonEmpty
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bifunctor
import           Data.Char
import           Data.Functor
import           Data.Functor.Identity
import           Data.List.NonEmpty                 (NonEmpty (..), (<|))
import           Polysemy
import           Polysemy.Error                     (Error, fromEither)
import           Polysemy.Reader
import           Text.Megaparsec                    hiding (parse, sepBy1,
                                                     sepEndBy1, some)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer         (indentGuard, indentLevel,
                                                     nonIndented)

import           Language.Dtfpl.Err
import           Language.Dtfpl.Module.Context
import           Language.Dtfpl.Parse.CustomError
import           Language.Dtfpl.Parse.Loc
import           Language.Dtfpl.Syntax

-- | Parser that has parsec functionality.
type PParsec = MonadParsec CustomError String

-- | Parser that keeps track of the current indentation level
-- from the start of the line.
type PIndentState = MonadState Pos

-- | Parse a module into its AST representation.
-- May throw parse errors in the error monad.
parse :: Members '[Reader ModuleContext, Error Err] r
    => String -> Sem r (A Mod 'Source)
parse input = do
    filename <- asks currentModulePathString
    fromEither =<<
        first ParseErr <$> evalStateT (runParserT mod_ filename input) pos1

-- | Parse a module into its AST representation.
-- For testing purposes only, so we don't have to deal with monad transformers.
_testParse :: String -> Either String (A Mod 'Source)
_testParse input = first errorBundlePretty $
    runIdentity $ evalStateT (runParserT (mod_ <* eof) "" input) pos1

-- | Parse a module.
mod_ :: (PParsec p, PIndentState p) => p (A Mod 'Source)
mod_ = addLoc (Mod
    <$> (T <$> many (try import_ <* scn))
    <*> (T <$> many (nonIndented scn tlDecl <* scn))) <* eof

-- | Parse an import statement.
import_ :: (PParsec p, PIndentState p) => p (A Import 'Source)
import_ = nonIndented scn $ addLoc $ Import U <$> (lexeme1 simport *> modName)

-- | Parse a module name.
modName :: PParsec p => p (A (P ModName) 'Source)
modName = addLoc $ P . ModName <$> modAtom `sepBy1` dot
  where modAtom = ModAtom <$> takeWhile1P Nothing isModAtomChar
        isModAtomChar c = (isLower c || c == '-')
                          && c `notElem` reservedChars

-- | Parse a top-level declaration.
tlDecl :: (PParsec p, PIndentState p) => p (A TopLevel 'Source)
tlDecl = addLoc $ TLDecl <$> option Priv (lexeme1 sexp $> Exp) <*> decl

-- | Parse a declaration.
decl :: (PParsec p, PIndentState p) => p (A Decl 'Source)
decl = def <|> let_
  where def = addLoc $ do
            lexeme1 sdef
            pAltBody <- snativeToExprBody
            Def <$> (IdentBind <$> ident) <*> indentBlock' (defAlt pAltBody)
        let_ = addLoc $ do
            lexeme1 slet
            pBody <- snativeToExprBody
            blockStart pBody $ Let <$> lexeme1 (IdentBind <$> ident) <* equals
        snativeToExprBody = maybe expr (const nativeExpr) <$>
            optional (lexeme1 snative)

-- | Parse a def alternative, given a parser for the body of the alternative
-- (either dtfpl expr or JS native expr)
defAlt :: (PParsec p, PIndentState p) =>
    (Pos -> p (A Expr 'Source)) -> p (A DefAlt 'Source)
defAlt body = addLoc $ blockMid body $
    DefAlt . T <$> (some (lexeme1 $ try pat) <* arrow)

-- | Parse a pattern.
pat :: PParsec p => p (A Pat 'Source)
pat = varPat <|> litPat <|> wildPat
  where varPat = addLoc $ VarPat . IdentBind <$> ident
        litPat = addLoc $ LitPat <$> literal
        wildPat = addLoc $ wildcard $> WildPat

-- | Given a parser for some node constructor that takes an 'Expr' as argument,
-- run that parser and run the 'expr' parser so that the continuation indent
-- for the expression must exceed the starting column of the parent node,
-- not the indent level of the start of the current line.
--
-- For example, this would parse:
--
-- > case foo of bar -> baz
--
-- So would this:
--
-- > case foo of bar ->
-- >                baz
--
-- But this would not parse:
--
-- > case foo of bar ->
-- >             baz
--
-- Because the indent for @baz@ is equal to, not greater than,
-- the starting indent of the parent 'CaseAlt' node.
exprBlockMid :: (PParsec p, PIndentState p) => p (A Expr 'Source -> b) -> p b
exprBlockMid = blockMid expr

-- | Given a parser for some node constructor that takes an 'Expr' as argument,
-- run that parser and run the 'expr' parser so that the continuation indent
-- for the expression must exceed the indent level of the start of the current
-- line, not the starting column of the parent node.
--
-- In the following example, the case alternative's starting column can be
-- less than the case's starting column, but it still parses successfully
-- because the CaseAlt's indent is greater than the indent level of the previous
-- line.
--
-- > def func
-- >     foo -> case foo of
-- >         bar -> baz
exprBlockStart :: (PParsec p, PIndentState p) => p (A Expr 'Source -> b) -> p b
exprBlockStart = blockStart expr

-- | Same as 'exprBlockMid' but generalized for any parser with the same type
-- as 'expr'.
blockMid :: (PParsec p, PIndentState p) =>
    (Pos -> p (A Expr 'Source)) -> p (A Expr 'Source -> b) -> p b
blockMid = blockWith indentLevel

-- | Same as 'exprBlockStart' but generalized for any parser with the same type
-- as 'expr'.
blockStart :: (PParsec p, PIndentState p) =>
    (Pos -> p (A Expr 'Source)) -> p (A Expr 'Source -> b) -> p b
blockStart = blockWith get

-- | Same as 'blockMid' and 'blockStart', but generalized for any method of
-- determining the current indentation.
blockWith :: (PParsec p, PIndentState p) =>
    p Pos -> (Pos -> p a) -> p (a -> b) -> p b
blockWith ip b p = do
    i <- ip
    p <*> (isc i *> b i)

-- | Parse an expression, where continuation lines must be indented greater
-- than the given 'Pos'.
-- This parser consumes any trailing whitespace at the end of the line.
expr :: (PParsec p, PIndentState p) => Pos -> p (A Expr 'Source)
expr i = foldl1 combine <$> term `sepEndBy1` try sc1'
  where combine f x = A (App f x) $ Loc (start (ann f)) (end (ann x))
        term = varExpr <|> if_ <|> case_ <|> lam <|> litExpr <|> par
        varExpr = addLoc $ VarExpr . IdentRef U <$> try ident
        litExpr = addLoc $ LitExpr <$> literal
        par = parens (sc' *> expr i)
        if_ = addLoc $ If
            <$> (sif *> sc1' *> expr i)
            <*> (sthen *> sc1' *> expr i)
            <*> (selse *> sc1' *> expr i)
        case_ = addLoc $ Case
            <$> (CaseHead . T <$>
                (scase *> sc1' *> expr i `sepBy1` lexeme0 comma <* sof))
            <*> indentBlock' caseAlt
        lam = addLoc $ fmap LamExpr $ exprBlockStart $ Lam . T
            <$> (lambda *> sc' *> some (lexeme1 $ try pat) <* arrow)
        sc' = isc i
        sc1' = isc1 i

-- | Parse a case alternative.
caseAlt :: (PParsec p, PIndentState p) => p (A CaseAlt 'Source)
caseAlt = addLoc $ exprBlockMid $ CaseAlt . T <$>
    (lexeme0 (try pat) `sepBy1` lexeme0 comma <* arrow)

-- | Parse a native JS expression and wrap it with 'NativeExpr' and 'A'.
nativeExpr :: PParsec p => Pos -> p (A Expr 'Source)
nativeExpr = addLoc . fmap NativeExpr . native

-- | Parse a native JS expression, where continuation lines must be indented
-- greater than the given 'Pos'.
-- This parser just eats all input and does not actually parse the JS
-- expression. The expression is parsed using a JS parser in the ParseNative
-- step.
native :: PParsec p => Pos -> p (Native 'Source)
native i = fmap Native $ addLoc $ P <$> nativeString
  where nativeString = do
            line <- takeWhile1P Nothing (`notElem` ['\r', '\n'])
            option line $ try $ do
                indent <- takeWhile1P Nothing isSpace
                col <- indentLevel
                if col > i
                    then ((line ++ indent) ++) <$> nativeString
                    else empty -- failure

-- Keyword strings

kimport :: String
kimport = "import"

kdef :: String
kdef = "def"

klet :: String
klet = "let"

kexp :: String
kexp = "exp"

knative :: String
knative = "native"

kif :: String
kif = "if"

kthen :: String
kthen = "then"

kelse :: String
kelse = "else"

kcase :: String
kcase = "case"

kof :: String
kof = "of"

-- | Strings which cannot be used as identifiers.
reservedWords :: [String]
reservedWords = [kdef, klet, kexp, knative, kif, kthen, kelse, kcase, kof]

-- | Parse a keyword.
keyword :: PParsec p => String -> p ()
keyword = string >>> (*> notFollowedBy (satisfy isIdentTailChar))

-- Keyword parsers

simport :: PParsec p => p ()
simport = keyword kimport

sdef :: PParsec p => p ()
sdef = keyword kdef

slet :: PParsec p => p ()
slet = keyword klet

sexp :: PParsec p => p ()
sexp = keyword kexp

snative :: PParsec p => p ()
snative = keyword knative

sif :: PParsec p => p ()
sif = keyword kif

sthen :: PParsec p => p ()
sthen = keyword kthen

selse :: PParsec p => p ()
selse = keyword kelse

scase :: PParsec p => p ()
scase = keyword kcase

sof :: PParsec p => p ()
sof = keyword kof

-- | Parse a symbol.
symbol :: PParsec p => String -> p ()
symbol = void . string

-- Symbol parsers

arrow :: PParsec p => p ()
arrow = symbol "->"

comma :: PParsec p => p ()
comma = symbol ","

dot :: PParsec p => p ()
dot = symbol "."

equals :: PParsec p => p ()
equals = symbol "="

lambda :: PParsec p => p ()
lambda = symbol "\\"

wildcard :: PParsec p => p ()
wildcard = symbol "_"

-- | Characters which cannot be used in identifiers.
{-# ANN reservedChars "HLint: ignore Use String" #-}
reservedChars :: [Char]
reservedChars = "()[]{}.,:;\\\""

-- | Run a parser inside parentheses.
parens :: PParsec p => p a -> p a
parens = between (string "(") (string ")")

-- | Parse an identifier.
ident :: PParsec p => p (A Ident 'Source)
ident = addLoc $ Ident <$> do
    identifier <- (:) <$> letterChar <*> takeWhileP Nothing isIdentTailChar
    if identifier `elem` reservedWords
        then customFailure $ ReservedWordIdentError identifier
        else pure identifier

-- | Check if the character can be used in the tail part of an identifier.
isIdentTailChar :: Char -> Bool
isIdentTailChar x = isPrint x
                 && not (isSeparator x)
                 && x `notElem` reservedChars

-- | Parse a literal.
literal :: PParsec p => p (A Lit 'Source)
literal = numLit <|> strLit
  where numLit = addLoc $ NumLit . read <$>
            (option id ((:) <$> char '-') <*>
                ((++) <$> digits <*>
                    option "" ((:) <$> char '.' <*> digits)))
          where digits = takeWhile1P (Just "digit") isDigit
        strLit = addLoc $ StrLit <$>
            (quote *> manyTill (escape <|> anySingleBut '\n') quote)
          where quote = char '"'
                escape = char '\\' *> choice
                    [ char '\\'
                    , char '"'
                    , char 'n' $> '\n' ]

-- | Turn a parser for an unannotated node into a parser for an annotated node.
addLoc :: PParsec p => p (n 'Source) -> p (A n 'Source)
addLoc p = do
    s <- getSourcePos
    A <$> p <*> (Loc s <$> getSourcePos)

-- | Parse at least one indented item, using the indent state for checking
-- indentation.
indentBlock' :: (PParsec p, PIndentState p) =>
    p (n 'Source) -> p (T NonEmpty n 'Source)
indentBlock' p = T <$> (get >>= isc1 >>= rest)
  where rest i' = do
            a <- p
            hasNext *> ((a <|) <$> rest i') <|> noMore $> a :| []
          where hasNext = try $ indentGuard scn1 EQ i'
                noMore = lookAhead $
                    try (scn *> eof) <|> void (indentGuard scn1 LT i')

-- | Space consumer without newlines. Accepts empty input.
sc0 :: PParsec p => p ()
sc0 = scWith takeWhileP

-- | Space consumer without newlines. Only succeeds if at least one space char.
sc1 :: PParsec p => p ()
sc1 = scWith takeWhile1P

-- | Create a space consumer with the given @takeWhile*P@ parser
scWith :: PParsec p => (Maybe String -> (Char -> Bool) -> p String) -> p ()
scWith p = hidden $ void $ p Nothing (== ' ')

-- | Space consumer with newlines.
scn :: (PParsec p, PIndentState p) => p ()
scn = scnWith space

-- | Space consumer with newlines. Only succeeds if at least one space char.
scn1 :: (PParsec p, PIndentState p) => p ()
scn1 = scnWith space1

-- | Run the space consumer and update the indent state when the line changes.
scnWith :: (PParsec p, PIndentState p) => p () -> p ()
scnWith p = do
    s <- getSourcePos
    hidden p
    e <- getSourcePos
    when (sourceLine s /= sourceLine e) $ put $ sourceColumn e

-- | Space consumer which only consumes newlines if the next line is indented
-- further than the given column.
isc :: (PParsec p, PIndentState p) => Pos -> p Pos
isc = indentGuard scn GT

-- | Same as 'isc' but only succeeds after parsing at least one char.
isc1 :: (PParsec p, PIndentState p) => Pos -> p Pos
isc1 = indentGuard scn1 GT

-- | Run the parser and consume any trailing whitespace.
lexeme0 :: PParsec p => p a -> p a
lexeme0 = (<* sc0)

-- | Run the parser and consume trailing whitespace.
-- There must be at least one space.
lexeme1 :: PParsec p => p a -> p a
lexeme1 = (<* sc1)
