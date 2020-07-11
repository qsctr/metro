module Language.Dtfpl.Module.Deps
    ( ModuleDeps
    ) where

import           Data.Map.Strict                 (Map)

import           Language.Dtfpl.Interface.Syntax
import           Language.Dtfpl.Syntax

type ModuleDeps = Map ModName IMod
