module Language.Dtfpl.Parser.Literal
    ( literal
    ) where

import           Data.Char
import           Data.Functor
import           Language.Dtfpl.Parser.Lexer
import           Language.Dtfpl.Syntax
import           Text.Megaparsec
import           Text.Megaparsec.Char

literal :: Parser (Literal ())
literal = numLit <|> strLit

numLit :: Parser (Literal ())
numLit = lexeme $ fmap (NumLit () . read) $
    ((option id $ (:) <$> char '-') <*>
        ((++) <$> digits <*>
            (option "" $ (:) <$> char '.' <*> digits)))
  where digits = takeWhile1P (Just "digit") isDigit

strLit :: Parser (Literal ())
strLit = lexeme $ fmap (StrLit ()) $
    quote *> manyTill (escape <|> anyChar) quote
  where quote = char '"'
        escape = char '\\' *> choice
            [ char '\\'
            , char '"'
            , char 'n' $> '\n' ]
