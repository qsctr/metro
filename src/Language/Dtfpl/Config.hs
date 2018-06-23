-- | Compiler configuration.
module Language.Dtfpl.Config
    ( Config (..)
    ) where

-- | Data type for compiler configuration.
data Config = Config {
    -- | Debug mode (i.e. not production).
    -- Compiler may be slower and perform extra checks.
    debug :: Bool
}
