{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Language.Dtfpl.Parser
    ( parse
    ) where

import           Control.Category                  ((>>>))
import           Control.Monad.Except
import           Control.Monad.State
import           Data.Bifunctor
import           Data.Char
import           Data.Functor
import           Data.Functor.Identity
import           Data.List
import           Data.List.NonEmpty                (NonEmpty (..), some1, (<|))
import           Text.Megaparsec                   hiding (parse)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer        (indentGuard, indentLevel,
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

parse :: MError m => String -> String -> m (A Prog 'Source)
parse filename input = liftEither =<<
    first ParseErr <$> evalStateT (runParserT prog filename input) pos1

_testParse :: String -> Either String (A Prog 'Source)
_testParse input = first parseErrorPretty $
    runIdentity $ evalStateT (runParserT prog "" input) pos1

prog :: (PParsec p, PIndentState p) => p (A Prog 'Source)
prog = addLoc (Prog . T <$> many (nonIndented scn decl <* scn)) <* eof

decl :: (PParsec p, PIndentState p) => p (A Decl 'Source)
decl = def <|> let_
  where def = addLoc $ Def <$> (lexeme sdef *> ident) <*> indentBlock' defAlt
        let_ = addLoc $ exprBlockMid $
            lexeme slet *> (Let <$> lexeme ident <* equals)

defAlt :: (PParsec p, PIndentState p) => p (A DefAlt 'Source)
defAlt = addLoc $ exprBlockMid $
    DefAlt . T <$> (some1 (lexeme $ try pat) <* arrow)

pat :: PParsec p => p (A Pat 'Source)
pat = varPat <|> litPat
  where varPat = addLoc $ VarPat <$> ident
        litPat = addLoc $ LitPat <$> literal

exprBlockMid :: (PParsec p, PIndentState p) => p (A Expr 'Source -> a) -> p a
exprBlockMid = exprBlockWith indentLevel

exprBlockStart :: (PParsec p, PIndentState p) => p (A Expr 'Source -> a) -> p a
exprBlockStart = exprBlockWith get

exprBlockWith :: (PParsec p, PIndentState p) =>
    p Pos -> p (A Expr 'Source -> a) -> p a
exprBlockWith ip p = do
    i <- ip
    p <*> (isc i *> expr i)

expr :: (PParsec p, PIndentState p) => Pos -> p (A Expr 'Source)
expr i = foldl1' combine <$> term `sepEndBy1` try sc1'
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
            <$> (CaseHead <$> (scase *> sc1' *> expr i <* sof))
            <*> indentBlock' caseAlt
        lam = addLoc $ fmap LamExpr $ exprBlockStart $ Lam . T
            <$> (lambda *> sc' *> some1 (lexeme $ try pat) <* arrow)
        sc' = isc i
        sc1' = isc1 i

caseAlt :: (PParsec p, PIndentState p) => p (A CaseAlt 'Source)
caseAlt = addLoc $ exprBlockMid $ CaseAlt <$> (lexeme pat <* arrow)

reservedWords :: [String]
reservedWords = ["def", "let", "if", "then", "else", "case", "of"]

sdef, slet, sif, sthen, selse, scase, sof :: PParsec p => p ()
[sdef, slet, sif, sthen, selse, scase, sof] = map
    (string >>> (*> notFollowedBy (satisfy isIdentTailChar)))
    reservedWords

arrow :: PParsec p => p ()
arrow = void $ string "->"

equals :: PParsec p => p ()
equals = void $ string "="

lambda :: PParsec p => p ()
lambda = void $ string "\\"

{-# ANN reservedChars "HLint: ignore Use String" #-}
reservedChars :: [Char]
reservedChars = "()[]{}.,:;\\\""

parens :: PParsec p => p a -> p a
parens = between (string "(") (string ")")

ident :: PParsec p => p (A Ident 'Source)
ident = addLoc $ Ident <$> do
    identifier <- (:) <$> letterChar <*> takeWhileP Nothing isIdentTailChar
    if identifier `elem` reservedWords
        then customFailure $ ReservedWordIdentError identifier
        else pure identifier

isIdentTailChar :: Char -> Bool
isIdentTailChar x = isPrint x
                 && not (isSeparator x)
                 && x `notElem` reservedChars

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

addLoc :: PParsec p => p (n 'Source) -> p (A n 'Source)
addLoc p = do
    s <- getPosition
    A <$> p <*> (Loc s <$> getPosition)

indentBlock' :: (PParsec p, PIndentState p) =>
    p (n 'Source) -> p (T NonEmpty n 'Source)
indentBlock' p = T <$> (get >>= isc1 >>= rest)
  where rest i' = do
            a <- p
            hasNext *> ((a <|) <$> rest i') <|> noMore $> a :| []
          where hasNext = try $ indentGuard scn1 EQ i'
                noMore = lookAhead $
                    try (scn *> eof) <|> void (indentGuard scn1 LT i')

sc :: PParsec p => p ()
sc = hidden $ void $ takeWhile1P Nothing (== ' ')

scn :: (PParsec p, PIndentState p) => p ()
scn = scnWith space

scn1 :: (PParsec p, PIndentState p) => p ()
scn1 = scnWith space1

scnWith :: (PParsec p, PIndentState p) => p () -> p ()
scnWith p = do
    s <- getPosition
    hidden p
    e <- getPosition
    when (sourceLine s /= sourceLine e) $ put $ sourceColumn e

isc :: (PParsec p, PIndentState p) => Pos -> p Pos
isc = indentGuard scn GT

isc1 :: (PParsec p, PIndentState p) => Pos -> p Pos
isc1 = indentGuard scn1 GT

lexeme :: PParsec p => p a -> p a
lexeme = (<* sc)
