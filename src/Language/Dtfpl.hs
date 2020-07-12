-- | Main dtfpl module.
module Language.Dtfpl
    ( compile
    ) where

import           Data.Bifunctor
import           Data.Function
import           Data.Functor
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           Polysemy.Resource

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err.Format
import           Language.Dtfpl.Module.Compile
import           Language.Dtfpl.Module.Context
import           Language.Dtfpl.Module.Output
import           Language.Dtfpl.NodeProc
import           Language.Dtfpl.Util.EPath
import           Language.Dtfpl.Util.FS

compile :: Config -> EFile -> IO (Either String ())
compile config path =
    compileModule
    & void
    & runError
    & fmap (first formatErr)
    & runOutputModule
    & runReader mainContext
    & runReader config
    & runNodeProc
    & runFS
    & resourceToIOFinal
    & embedToFinal
    & runFinal
  where mainContext = ModuleContext
            { currentModulePath = path
            , isMainModule = True }
