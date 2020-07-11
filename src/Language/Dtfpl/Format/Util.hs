module Language.Dtfpl.Format.Util
    ( formatQuote
    , parensLoc
    ) where

import           Language.Dtfpl.Err.Util
import           Language.Dtfpl.Format
import           Language.Dtfpl.Parse.Loc

-- | Format a node then wrap with quotes.
-- Useful for showing AST nodes in error messages.
formatQuote :: Format a => a -> String
formatQuote = errQuote . format

parensLoc :: Maybe Loc -> String
parensLoc (Just loc) = " (" ++ format loc ++ ")"
parensLoc Nothing    = ""
