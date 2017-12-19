{-# LANGUAGE ViewPatterns       #-}

module Language.Dtfpl.Parser where

import           Control.Category           ((>>>))
import           Data.Char
import           Data.Functor
import           Data.List
import           Language.Dtfpl.Syntax
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (IndentOpt (..), indentBlock,
                                             indentGuard, indentLevel,
                                             nonIndented)

type Parser = Parsec PError String

type NodeParser a = Parser (a Loc)

data Loc = Loc SourcePos SourcePos deriving Show

data PError
    = ReservedWordIdentError String
    deriving (Eq, Ord, Show)

instance ShowErrorComponent PError where
    showErrorComponent (ReservedWordIdentError reservedWord) =
        reservedWord ++ " is a reserved word"

prog :: NodeParser Prog
prog = addLoc (Prog <$> many (nonIndented scn decl <* scn)) <* eof

decl :: NodeParser Decl
decl = def <|> let_
  where def = addLoc $ indentBlock scn $ do
            lexeme sdef
            name <- ident
            pure $ IndentSome Nothing (pure . Def name) defAlt
        let_ = exprBlock $
            lexeme slet *> (Let <$> lexeme ident <* equals)

defAlt :: NodeParser DefAlt
defAlt = exprBlock $ DefAlt <$> (some (lexeme pat) <* arrow)

pat :: NodeParser Pat
pat = addLoc $ VarPat <$> ident

exprBlock :: Parser (Expr Loc -> Loc -> a) -> Parser a
exprBlock p = do
    i <- indentLevel
    start <- getPosition
    header <- p
    e <- isc i *> expr i <* lookAhead (scn1 <|> eof)
    pure $ header e $ Loc start $ endPos e

expr :: Pos -> NodeParser Expr
expr i = app <|> term
  where term =  addLoc (Var <$> ident)
            <|> addLoc (Lit <$> literal)
            <|> parens (sc' *> expr i)
        app = foldl1' (\f x -> App f x $ Loc (startPos f) (endPos x))
            <$> term `sepEndBy1` try sc1'
        sc' = isc i
        sc1' = isc1 i

reservedWords :: [String]
reservedWords = ["def", "let"]

sdef, slet :: Parser ()
[sdef, slet] = map
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

ident :: NodeParser Ident
ident = addLoc $ Ident <$> do
    identifier <- (:) <$> letterChar <*> takeWhileP Nothing isIdentTailChar
    if identifier `elem` reservedWords
        then customFailure $ ReservedWordIdentError identifier
        else return identifier

isIdentTailChar :: Char -> Bool
isIdentTailChar x = isPrint x
                 && not (isSeparator x)
                 && x `notElem` reservedChars

literal :: NodeParser Literal
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

startPos, endPos :: Ann n => n Loc -> SourcePos
startPos (ann -> Loc start _) = start
endPos (ann -> Loc _ end) = end

addLoc :: Parser (Loc -> a) -> Parser a
addLoc p = do
    start <- getPosition
    ctor <- p
    ctor . Loc start <$> getPosition

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
