module Language.Dtfpl.Parser.Ident
    ( ident
    ) where

import           Data.Char
import           Language.Dtfpl.Parser.Lexer
import           Language.Dtfpl.Syntax
import           Text.Megaparsec
import           Text.Megaparsec.Char

reservedWords :: [String]
reservedWords = []

reservedChars :: [Char]
reservedChars = "()[]{}.,:\\\""

ident :: Parser (Ident ())
ident = lexeme $ fmap (Ident ()) $ do
    i <- (:) <$> letterChar <*> takeWhileP Nothing isRestChar
    if i `elem` reservedWords
        then fail $ i ++ " is a reserved word"
        else return i
  where isRestChar x = isPrint x
                    && not (isSeparator x)
                    && x `notElem` reservedChars
