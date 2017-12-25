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
import           Text.Megaparsec.Char.Lexer       (IndentOpt (..), indentBlock,
                                                   indentGuard, indentLevel,
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
    first ParsingErr <$> runParserT prog filename input

testParse :: String -> Either String (AProg Loc)
testParse input = first parseErrorPretty $
    runReader (runParserT prog "" input) undefined

prog :: LocParser' Prog
prog = addLoc (Prog <$> many (nonIndented scn decl <* scn)) <* eof

decl :: LocParser' Decl
decl = def <|> let_
  where def = indentBlock scn $ do
            s <- getPosition
            lexeme sdef
            name <- ident
            let result alts = pure $ A (Def name alts) $
                    Loc s $ end $ ann $ last alts
            pure $ IndentSome Nothing result defAlt
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
expr i = app <|> notApp
  where notApp = varExpr <|> if_ <|> litExpr <|> par
        varExpr = addLoc $ VarExpr <$> try ident
        litExpr = addLoc $ LitExpr <$> literal
        par = parens (sc' *> expr i)
        if_ = addLoc $ If
            <$> (sif *> sc1' *> expr i)
            <*> (sthen *> sc1' *> expr i)
            <*> (selse *> sc1' *> expr i)
        app = foldl1' combine <$> notApp `sepEndBy1` try sc1'
          where combine f x = A (App f x) $ Loc (start (ann f)) (end (ann x))
        sc' = isc i
        sc1' = isc1 i

reservedWords :: [String]
reservedWords = ["def", "let", "if", "then", "else"]

sdef, slet, sif, sthen, selse :: Parser ()
[sdef, slet, sif, sthen, selse] = map
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
        else return identifier

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

sc :: Parser ()
sc = hidden $ void $ takeWhile1P Nothing (== ' ')

scn :: Parser ()
scn = hidden space

scn1 :: Parser ()
scn1 = hidden space1

isc :: Pos -> Parser ()
isc = void . indentGuard scn GT

isc1 :: Pos -> Parser ()
isc1 = void . indentGuard scn1 GT

lexeme :: Parser a -> Parser a
lexeme = (<* sc)
