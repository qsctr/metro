module Language.Metro.Module.Deps
    ( ModuleDeps
    ) where

import           Data.Map.Strict                 (Map)

import           Language.Metro.Interface.Syntax
import           Language.Metro.Syntax

type ModuleDeps = Map ModName IMod
