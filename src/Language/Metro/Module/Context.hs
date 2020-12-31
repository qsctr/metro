{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Metro.Module.Context
    ( ModuleContext (..)
    , currentModulePathString
    , isMainModule
    ) where

import           Polysemy
import           Polysemy.Reader
import qualified System.Path                as P

import           Language.Metro.Config
import           Language.Metro.Syntax
import           Language.Metro.Util.CEPath
import           Language.Metro.Util.EPath
import           Language.Metro.Util.FS

data ModuleContext = ModuleContext {
    currentModulePath :: CEFile,
    importStack       :: [A Import 'ModResolved]
}

currentModulePathString :: ModuleContext -> String
currentModulePathString (ePath . currentModulePath -> EPath path) =
    P.toString path

isMainModule :: Members '[Reader ModuleContext, Reader Config, FS] r
    => Sem r Bool
isMainModule = (==)
    <$> asks (cPath . currentModulePath)
    <*> asks (cPath . mainModulePath)
