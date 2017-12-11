module Language.Dtfpl.Parser.Lexer
    ( Parser
    , lexeme
    , symbol
    , parens ) where

import           Control.Applicative
import           Control.Monad
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

scn :: Parser ()
scn = L.space space1 empty empty

sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing (== ' ')) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
