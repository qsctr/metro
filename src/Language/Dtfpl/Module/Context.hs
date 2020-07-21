{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Dtfpl.Module.Context
    ( ModuleContext (..)
    , currentModulePathString
    , isMainModule
    ) where

import           Polysemy
import           Polysemy.Reader
import qualified System.Path                as P

import           Language.Dtfpl.Config
import           Language.Dtfpl.Syntax
import           Language.Dtfpl.Util.CEPath
import           Language.Dtfpl.Util.EPath
import           Language.Dtfpl.Util.FS

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
