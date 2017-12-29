module Language.Dtfpl.Parse.Parser
    ( parse
    ) where

import           Control.Category                 ((>>>))
import           Data.Bifunctor
import           Data.Char
import           Data.Functor
import           Data.List
import           Text.Megaparsec                  hiding (parse)
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer       (indentGuard, indentLevel,
                                                   nonIndented)

import           Language.Dtfpl.M
import           Language.Dtfpl.Parse.CustomError
import           Language.Dtfpl.Parse.Loc
import           Language.Dtfpl.Syntax

type Parser = ParsecT CustomError String (Reader Config)

type LocParser n = Parser (A n Loc)

type LocParser' n = Parser (A' n Loc)

parse :: String -> String -> M (AProg Loc)
parse filename input = ExceptT $
    first ParseErr <$> runParserT prog filename input

testParse :: String -> Either String (AProg Loc)
testParse input = first parseErrorPretty $
    runReader (runParserT prog "" input) undefined

prog :: LocParser' Prog
prog = addLoc (Prog <$> many (nonIndented scn decl <* scn)) <* eof

decl :: LocParser' Decl
decl = def <|> let_
  where def = addLoc $ do
            i <- indentLevel
            Def <$> (lexeme sdef *> ident) <*> indentBlock' i defAlt
        let_ = addLoc $ exprBlock $
            lexeme slet *> (Let <$> lexeme ident <* equals)

defAlt :: LocParser' DefAlt
defAlt = addLoc $ exprBlock $ DefAlt <$> (some (lexeme $ try pat) <* arrow)

pat :: LocParser' Pat
pat = varPat <|> litPat
  where varPat = addLoc $ VarPat <$> ident
        litPat = addLoc $ LitPat <$> literal

exprBlock :: Parser (A' Expr Loc -> a) -> Parser a
exprBlock p = do
    i <- indentLevel
    p <*> (isc i *> expr i <* lookAhead (scn1 <|> hidden eof))

expr :: Pos -> LocParser' Expr
expr i = app
  where notApp = varExpr <|> if_ <|> case_ <|> litExpr <|> par
        varExpr = addLoc $ VarExpr <$> try ident
        litExpr = addLoc $ LitExpr <$> literal
        par = parens (sc' *> expr i)
        if_ = addLoc $ If
            <$> (sif *> sc1' *> expr i)
            <*> (sthen *> sc1' *> expr i)
            <*> (selse *> sc1' *> expr i)
        case_ = addLoc $ Case
            <$> (scase *> sc1' *> expr i <* sof)
            <*> indentBlock' i caseAlt
        app = foldl1' combine <$> notApp `sepEndBy1` try sc1'
          where combine f x = A (App f x) $ Loc (start (ann f)) (end (ann x))
        sc' = isc i
        sc1' = isc1 i

caseAlt :: LocParser' CaseAlt
caseAlt = addLoc $ exprBlock $ CaseAlt <$> (lexeme pat <* arrow)

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

{-# ANN reservedChars "HLint: ignore Use String" #-}
reservedChars :: [Char]
reservedChars = "()[]{}.,:;\\\""

parens :: Parser a -> Parser a
parens = between (string "(") (string ")")

ident :: LocParser Ident
ident = addLoc $ Ident <$> do
    identifier <- (:) <$> letterChar <*> takeWhileP Nothing isIdentTailChar
    if identifier `elem` reservedWords
        then customFailure $ ReservedWordIdentError identifier
        else pure identifier

isIdentTailChar :: Char -> Bool
isIdentTailChar x = isPrint x
                 && not (isSeparator x)
                 && x `notElem` reservedChars

literal :: LocParser Lit
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

addLoc :: Parser n -> LocParser n
addLoc p = do
    s <- getPosition
    A <$> p <*> (Loc s <$> getPosition)

indentBlock' :: Pos -> Parser a -> Parser [a]
indentBlock' i p = isc1 i >>= rest
  where rest i' = do
            a <- p
            hasNext *> ((a :) <$> rest i') <|> noMore $> [a]
          where hasNext = try $ indentGuard scn1 EQ i'
                noMore = lookAhead $
                    try (scn *> eof) <|> (void $ indentGuard scn1 LT i')

sc :: Parser ()
sc = hidden $ void $ takeWhile1P Nothing (== ' ')

scn :: Parser ()
scn = hidden space

scn1 :: Parser ()
scn1 = hidden space1

isc :: Pos -> Parser Pos
isc = indentGuard scn GT

isc1 :: Pos -> Parser Pos
isc1 = indentGuard scn1 GT

lexeme :: Parser a -> Parser a
lexeme = (<* sc)
