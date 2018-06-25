-- | Custom parser errors.
module Language.Dtfpl.Parser.CustomError
    ( CustomError (..)
    ) where

import Text.Megaparsec.Error

-- | Custom parser error.
data CustomError
    -- | Reserved word used as identifier.
    = ReservedWordIdentError String
    deriving (Eq, Ord, Show)

instance ShowErrorComponent CustomError where
    showErrorComponent (ReservedWordIdentError reservedWord) =
        reservedWord ++ " is a reserved word"
