-- | Compiler configuration.
module Language.Dtfpl.Config
    ( Config (..)
    ) where

import           Language.Dtfpl.Util.EPath

-- | Data type for compiler configuration.
data Config = Config {
    -- | Debug mode (i.e. not production).
    -- Compiler may be slower and perform extra checks.
    debug             :: Bool,
    -- | Directories in which to search for modules to load.
    moduleSearchPaths :: [EDir]
}
