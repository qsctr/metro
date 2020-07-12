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
    -- | The main file being compiled (e.g. passed as command line argument).
    mainModulePath    :: EFile,
    -- | Directories in which to search for modules to load.
    moduleSearchPaths :: [EDir]
}
