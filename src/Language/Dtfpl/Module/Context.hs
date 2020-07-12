{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Dtfpl.Module.Context
    ( ModuleContext (..)
    , currentModulePathString
    , isMainModule
    ) where

import           Polysemy
import           Polysemy.Reader
import qualified System.Path               as P

import           Language.Dtfpl.Config
import           Language.Dtfpl.Util.EPath
import           Language.Dtfpl.Util.FS

data ModuleContext = ModuleContext {
    currentModulePath :: EFile
}

currentModulePathString :: ModuleContext -> String
currentModulePathString (currentModulePath -> EPath path) = P.toString path

isMainModule :: Members '[Reader ModuleContext, Reader Config, FS] r
    => Sem r Bool
isMainModule = (==) <$> (asks currentModulePath >>= fsCanonicalizePath)
                    <*> (asks mainModulePath >>= fsCanonicalizePath)
