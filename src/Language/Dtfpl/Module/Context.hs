{-# LANGUAGE ViewPatterns #-}

module Language.Dtfpl.Module.Context
    ( ModuleContext (..)
    , currentModulePathString
    ) where

import qualified System.Path               as P

import           Language.Dtfpl.Util.EPath

data ModuleContext = ModuleContext {
    currentModulePath :: EFile,
    isMainModule      :: Bool
}

currentModulePathString :: ModuleContext -> String
currentModulePathString (currentModulePath -> EPath path) = P.toString path
