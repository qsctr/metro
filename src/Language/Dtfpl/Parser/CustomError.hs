module Language.Dtfpl.Parser.CustomError
    ( CustomError (..)
    ) where

import Text.Megaparsec.Error

data CustomError
    = ReservedWordIdentError String
    deriving (Eq, Ord, Show)

instance ShowErrorComponent CustomError where
    showErrorComponent (ReservedWordIdentError reservedWord) =
        reservedWord ++ " is a reserved word"
