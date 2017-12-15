module Language.Dtfpl.Parser.Lexer
    ( Parser
    , sc
    , scn
    , lexeme
    , symbol
    , parens
    ) where

import           Control.Applicative
import           Data.Functor
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing (== ' ')) empty empty

scn :: Parser ()
scn = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (string "(") (string ")")
