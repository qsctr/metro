{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

-- | Parser for dtfpl.
module Language.Dtfpl.Parser
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
import           Text.Megaparsec                    hiding (parse, sepEndBy1,
                                                     some, sepBy1)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer         (indentGuard, indentLevel,
                                                     nonIndented)

import           Language.Dtfpl.Err
import           Language.Dtfpl.M
import           Language.Dtfpl.Parser.CustomError
import           Language.Dtfpl.Parser.Loc
import           Language.Dtfpl.Syntax

-- | Parser that has parsec functionality.
type PParsec = MonadParsec CustomError String

-- | Parser that keeps track of the current indentation level
-- from the start of the line.
type PIndentState = MonadState Pos

-- | Parse a program into its AST representation.
-- May throw parse errors in the error monad.
parse :: MError m => FilePath -> String -> m (A Prog 'Source)
parse filename input = liftEither =<<
    first ParseErr <$> evalStateT (runParserT prog filename input) pos1

-- | Parse a program into its AST representation.
-- For testing purposes only, so we don't have to deal with monad transformers.
_testParse :: String -> Either String (A Prog 'Source)
_testParse input = first parseErrorPretty $
    runIdentity $ evalStateT (runParserT (prog <* eof) "" input) pos1

-- | Parse a program.
prog :: (PParsec p, PIndentState p) => p (A Prog 'Source)
prog = addLoc (Prog . T <$> many (nonIndented scn decl <* scn)) <* eof

-- | Parse a declaration.
decl :: (PParsec p, PIndentState p) => p (A Decl 'Source)
decl = def <|> let_
  where def = addLoc $ Def <$> (lexeme1 sdef *> ident) <*> indentBlock' defAlt
        let_ = addLoc $ exprBlockMid $
            lexeme1 slet *> (Let <$> lexeme1 ident <* equals)

-- | Parse a def alternative.
defAlt :: (PParsec p, PIndentState p) => p (A DefAlt 'Source)
defAlt = addLoc $ exprBlockMid $
    DefAlt . T <$> (some (lexeme1 $ try pat) <* arrow)

-- | Parse a pattern.
pat :: PParsec p => p (A Pat 'Source)
pat = varPat <|> litPat
  where varPat = addLoc $ VarPat <$> ident
        litPat = addLoc $ LitPat <$> literal

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
exprBlockMid :: (PParsec p, PIndentState p) => p (A Expr 'Source -> a) -> p a
exprBlockMid = exprBlockWith indentLevel

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
exprBlockStart :: (PParsec p, PIndentState p) => p (A Expr 'Source -> a) -> p a
exprBlockStart = exprBlockWith get

-- | Given a parser for some node constructor that takes an 'Expr' as argument,
-- and some method of determining the current indentation, run that parser and
-- run the 'expr' parser using the given indentation method to determine the
-- minimum continuation indent.
exprBlockWith :: (PParsec p, PIndentState p) =>
    p Pos -> p (A Expr 'Source -> a) -> p a
exprBlockWith ip p = do
    i <- ip
    p <*> (isc i *> expr i)

-- | Parse an expression, where continuation lines must be indented greater
-- than the given 'Pos'.
-- This parser consumes any trailing whitespace at the end of the line.
expr :: (PParsec p, PIndentState p) => Pos -> p (A Expr 'Source)
expr i = foldl1 combine <$> term `sepEndBy1` try sc1'
  where combine f x = A (App f x) $ Loc (start (ann f)) (end (ann x))
        term = varExpr <|> if_ <|> case_ <|> lam <|> litExpr <|> par
        varExpr = addLoc $ VarExpr <$> try ident
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

-- | Keyword string.
kdef, klet, kif, kthen, kelse, kcase, kof :: String
kdef = "def"
klet = "let"
kif = "if"
kthen = "then"
kelse = "else"
kcase = "case"
kof = "of"

-- | Strings which cannot be used as identifiers.
reservedWords :: [String]
reservedWords = [kdef, klet, kif, kthen, kelse, kcase, kof]

-- | Parse a keyword.
keyword :: PParsec p => String -> p ()
keyword = string >>> (*> notFollowedBy (satisfy isIdentTailChar))

-- | Keyword parser.
sdef, slet, sif, sthen, selse, scase, sof :: PParsec p => p ()
sdef = keyword kdef
slet = keyword klet
sif = keyword kif
sthen = keyword kthen
selse = keyword kelse
scase = keyword kcase
sof = keyword kof

-- | Parse a symbol.
symbol :: PParsec p => String -> p ()
symbol = void . string

-- | Symbol parser.
arrow, comma, equals, lambda :: PParsec p => p ()
arrow = symbol "->"
comma = symbol ","
equals = symbol "="
lambda = symbol "\\"

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
            (quote *> manyTill (escape <|> notChar '\n') quote)
          where quote = char '"'
                escape = char '\\' *> choice
                    [ char '\\'
                    , char '"'
                    , char 'n' $> '\n' ]

-- | Turn a parser for an unannotated node into a parser for an annotated node.
addLoc :: PParsec p => p (n 'Source) -> p (A n 'Source)
addLoc p = do
    s <- getPosition
    A <$> p <*> (Loc s <$> getPosition)

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
    s <- getPosition
    hidden p
    e <- getPosition
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
