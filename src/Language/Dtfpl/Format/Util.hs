module Language.Dtfpl.Format.Util
    ( formatQuote
    , parens
    , formatParens
    , parensLoc
    ) where

import           Language.Dtfpl.Err.Util
import           Language.Dtfpl.Format
import           Language.Dtfpl.Parse.Loc

-- | Format a node then wrap with quotes.
-- Useful for showing AST nodes in error messages.
formatQuote :: Format a => a -> String
formatQuote = errQuote . format

parens :: String -> String
parens s = " (" ++ s ++ ")"

formatParens :: Format a => a -> String
formatParens = parens . format

parensLoc :: Maybe Loc -> String
parensLoc (Just loc) = formatParens loc
parensLoc Nothing    = ""
