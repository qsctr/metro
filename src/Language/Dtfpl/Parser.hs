{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Dtfpl.Parser
    ( parse
    ) where

import           Control.Category                  ((>>>))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Bifunctor
import           Data.Char
import           Data.Functor
import           Data.List
import           Data.List.NonEmpty                (NonEmpty (..), some1, (<|))
import           Text.Megaparsec                   hiding (parse)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer        (indentGuard, indentLevel,
                                                    nonIndented)

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err
import           Language.Dtfpl.M
import           Language.Dtfpl.Parser.CustomError
import           Language.Dtfpl.Parser.Loc
import           Language.Dtfpl.Syntax

type Parser = ParsecT CustomError String (StateT Pos (Reader Config))

parse :: String -> String -> M (A Prog 'Source)
parse filename input = ExceptT $
    first ParseErr <$> evalStateT (runParserT prog filename input) pos1

testParse :: String -> Either String (A Prog 'Source)
testParse input = first parseErrorPretty $
    runReader (evalStateT (runParserT prog "" input) pos1)
        Config { debug = True }

prog :: Parser (A Prog 'Source)
prog = addLoc (Prog . T <$> many (nonIndented scn decl <* scn)) <* eof

decl :: Parser (A Decl 'Source)
decl = def <|> let_
  where def = addLoc $ Def <$> (lexeme sdef *> ident) <*> indentBlock' defAlt
        let_ = addLoc $ exprBlockMid $
            lexeme slet *> (Let <$> lexeme ident <* equals)

defAlt :: Parser (A DefAlt 'Source)
defAlt = addLoc $ exprBlockMid $ DefAlt . T <$> (some1 (lexeme $ try pat) <* arrow)

pat :: Parser (A Pat 'Source)
pat = varPat <|> litPat
  where varPat = addLoc $ VarPat <$> ident
        litPat = addLoc $ LitPat <$> literal

exprBlockMid :: Parser (A Expr 'Source -> a) -> Parser a
exprBlockMid = exprBlockWith indentLevel

exprBlockStart :: Parser (A Expr 'Source -> a) -> Parser a
exprBlockStart = exprBlockWith get

exprBlockWith :: Parser Pos -> Parser (A Expr 'Source -> a) -> Parser a
exprBlockWith ip p = do
    i <- ip
    p <*> (isc i *> expr i)

expr :: Pos -> Parser (A Expr 'Source)
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

caseAlt :: Parser (A CaseAlt 'Source)
caseAlt = addLoc $ exprBlockMid $ CaseAlt <$> (lexeme pat <* arrow)

reservedWords :: [String]
reservedWords = ["def", "let", "if", "then", "else", "case", "of"]

sdef, slet, sif, sthen, selse, scase, sof :: Parser ()
[sdef, slet, sif, sthen, selse, scase, sof] = map
    (string >>> (*> notFollowedBy (satisfy isIdentTailChar)))
    reservedWords

arrow :: Parser ()
arrow = void $ string "->"

equals :: Parser ()
equals = void $ string "="

lambda :: Parser ()
lambda = void $ string "\\"

{-# ANN reservedChars "HLint: ignore Use String" #-}
reservedChars :: [Char]
reservedChars = "()[]{}.,:;\\\""

parens :: Parser a -> Parser a
parens = between (string "(") (string ")")

ident :: Parser (A Ident 'Source)
ident = addLoc $ Ident <$> do
    identifier <- (:) <$> letterChar <*> takeWhileP Nothing isIdentTailChar
    if identifier `elem` reservedWords
        then customFailure $ ReservedWordIdentError identifier
        else pure identifier

isIdentTailChar :: Char -> Bool
isIdentTailChar x = isPrint x
                 && not (isSeparator x)
                 && x `notElem` reservedChars

literal :: Parser (A Lit 'Source)
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

addLoc :: Parser (n 'Source) -> Parser (A n 'Source)
addLoc p = do
    s <- getPosition
    A <$> p <*> (Loc s <$> getPosition)

indentBlock' :: Parser (n p) -> Parser (T NonEmpty n p)
indentBlock' p = T <$> (get >>= isc1 >>= rest)
  where rest i' = do
            a <- p
            hasNext *> ((a <|) <$> rest i') <|> noMore $> a :| []
          where hasNext = try $ indentGuard scn1 EQ i'
                noMore = lookAhead $
                    try (scn *> eof) <|> (void $ indentGuard scn1 LT i')

sc :: Parser ()
sc = hidden $ void $ takeWhile1P Nothing (== ' ')

scn :: Parser ()
scn = scnWith space

scn1 :: Parser ()
scn1 = scnWith space1

scnWith :: Parser () -> Parser ()
scnWith p = do
    s <- getPosition
    hidden p
    e <- getPosition
    when (sourceLine s /= sourceLine e) $ put $ sourceColumn e

isc :: Pos -> Parser Pos
isc = indentGuard scn GT

isc1 :: Pos -> Parser Pos
isc1 = indentGuard scn1 GT

lexeme :: Parser a -> Parser a
lexeme = (<* sc)
