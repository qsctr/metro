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

data PError
    = ReservedWordIdentError String
    deriving (Eq, Ord)

instance ShowErrorComponent PError where
    showErrorComponent (ReservedWordIdentError reservedWord) =
        reservedWord ++ " is a reserved word"

prog :: Parser (Prog ())
prog = Prog () <$> many (nonIndented scn decl <* scn) <* eof

decl :: Parser (Decl ())
decl = def
  where def = indentBlock scn $ do
            lexeme sdef
            name <- ident
            pure $ IndentSome Nothing (pure . Def () name) defAlt

defAlt :: Parser (DefAlt ())
defAlt = do
    i <- indentLevel
    DefAlt () <$> some (lexeme pat) <*>
        (arrow *> isc i *> expr i <* lookAhead (scn1 <|> eof))

pat :: Parser (Pat ())
pat = VarPat () <$> ident

expr :: Pos -> Parser (Expr ())
expr i = expr'
  where expr' = app <|> term
        term =  Var () <$> ident
            <|> Lit () <$> literal
            <|> parens (sc' *> expr')
        app = foldl1' (App ()) <$> term `sepEndBy1` try sc1'
        sc' = isc i
        sc1' = isc1 i

reservedWords :: [String]
reservedWords = ["def"]

sdef :: Parser ()
[sdef] = map
    (string >>> (*> notFollowedBy (satisfy isIdentTailChar)))
    reservedWords

arrow :: Parser ()
arrow = void $ string "->"

{-# ANN reservedChars "HLint: ignore Use String" #-}
reservedChars :: [Char]
reservedChars = "()[]{}.,:;\\\""

parens :: Parser a -> Parser a
parens = between (string "(") (string ")")

ident :: Parser (Ident ())
ident = Ident () <$> do
    identifier <- (:) <$> letterChar <*> takeWhileP Nothing isIdentTailChar
    if identifier `elem` reservedWords
        then customFailure $ ReservedWordIdentError identifier
        else return identifier

isIdentTailChar :: Char -> Bool
isIdentTailChar x = isPrint x
                 && not (isSeparator x)
                 && x `notElem` reservedChars

literal :: Parser (Literal ())
literal = numLit <|> strLit
  where numLit = NumLit () . read <$>
            (option id ((:) <$> char '-') <*>
                ((++) <$> digits <*>
                    option "" ((:) <$> char '.' <*> digits)))
          where digits = takeWhile1P (Just "digit") isDigit
        strLit = StrLit () <$>
            (quote *> manyTill (escape <|> notChar '\n') quote)
          where quote = char '"'
                escape = char '\\' *> choice
                    [ char '\\'
                    , char '"'
                    , char 'n' $> '\n' ]

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
