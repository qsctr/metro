-- | Main metro module.
module Language.Metro
    ( compile
    ) where

import           Data.Bifunctor
import           Data.Function
import           Data.Functor
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           Polysemy.Resource

import           Language.Metro.Config
import           Language.Metro.Err.Format
import           Language.Metro.Log
import           Language.Metro.Module.Cache
import           Language.Metro.Module.Compile
import           Language.Metro.Module.Context
import           Language.Metro.Module.ModFS
import           Language.Metro.NodeProc
import           Language.Metro.Util.FS

compile :: Config -> IO (Either String ())
compile config =
    compileModule
    & void
    & runModFS
    & runModuleCache
    & runError
    & fmap (first formatErr)
    & runReader mainContext
    & runLog
    & runReader config
    & runNodeProc
    & runFS
    & resourceToIOFinal
    & embedToFinal
    & runFinal
  where mainContext = ModuleContext
            { currentModulePath = mainModulePath config
            , importStack = [] }
