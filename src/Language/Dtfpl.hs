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
import           Language.Dtfpl.Module.Load
import           Language.Dtfpl.Module.Output
import           Language.Dtfpl.NodeProc
import           Language.Dtfpl.Util.EPath

compile :: Config -> EFile -> IO (Either String ())
compile config path =
    compileModule
    & void
    & runError
    & fmap (first formatErr)
    & runLoadModule
    & runOutputModule
    & runReader ModuleContext { currentModulePath = path }
    & runReader config
    & runNodeProc
    & resourceToIOFinal
    & embedToFinal
    & runFinal
