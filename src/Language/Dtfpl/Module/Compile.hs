{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}

module Language.Dtfpl.Module.Compile
    ( compileModule
    ) where

import qualified Data.Map.Strict                   as M
import           Data.Traversable
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err
import           Language.Dtfpl.Generate
import           Language.Dtfpl.Interface.Generate
import           Language.Dtfpl.Interface.Syntax
import           Language.Dtfpl.Module.Context
import           Language.Dtfpl.Module.Load
import           Language.Dtfpl.Module.Output
import           Language.Dtfpl.Module.Resolve
import           Language.Dtfpl.NodeProc
import           Language.Dtfpl.Parse
import           Language.Dtfpl.Render
import           Language.Dtfpl.Simplify
import           Language.Dtfpl.Syntax.Util

compileModule :: Members '[LoadModule, OutputModule, Reader ModuleContext,
    Reader Config, Error Err, NodeProc] r => Sem r IMod
compileModule = do
    src <- loadModule
    ast <- parse src >>= resolveImports
    deps <- M.fromList <$> for (splitImports ast) \(path, modName) -> do
        let context = ModuleContext { currentModulePath = path }
        iMod <- local (const context) compileModule
        pure (modName, iMod)
    core <- runReader deps $ simplify ast
    js <- generate core >>= render
    outputModule js
    pure $ generateInterface core
