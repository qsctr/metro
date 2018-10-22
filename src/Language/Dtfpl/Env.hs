module Language.Dtfpl.Env
    ( Env (..)
    ) where

import           Language.Dtfpl.Config
import           Language.Dtfpl.NodeProc

data Env = Env {
    config   :: Config,
    nodeProc :: NodeProc
}
