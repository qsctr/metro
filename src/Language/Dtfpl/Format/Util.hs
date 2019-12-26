module Language.Dtfpl.Format.Util
    ( formatQuote
    ) where

import           Language.Dtfpl.Err.Util
import           Language.Dtfpl.Format

-- | Format a node then wrap with quotes.
-- Useful for showing AST nodes in error messages.
formatQuote :: Format a => a -> String
formatQuote = errQuote . format
