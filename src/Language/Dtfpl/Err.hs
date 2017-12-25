module Language.Dtfpl.Err
    ( Err (..)
    ) where

import Text.Megaparsec.Error

import Language.Dtfpl.Parse.CustomError

data Err
    = ParsingErr (ParseError Char CustomError)
    deriving Show
