module Language.Metro.Err.Util
    ( errQuote
    ) where

-- | Wrap a string with backticks.
-- Useful for showing pieces of code in error messages.
errQuote :: String -> String
errQuote = ('`' :) . (++ "`")
