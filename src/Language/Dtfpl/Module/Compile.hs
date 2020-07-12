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
import           Language.Dtfpl.Module.Output
import           Language.Dtfpl.Module.Resolve
import           Language.Dtfpl.NodeProc
import           Language.Dtfpl.Parse
import           Language.Dtfpl.Render
import           Language.Dtfpl.Simplify
import           Language.Dtfpl.Syntax.Util
import           Language.Dtfpl.Util.FS

compileModule :: Members '[OutputModule, Reader ModuleContext,
    Reader Config, Error Err, NodeProc, FS] r => Sem r IMod
compileModule = do
    src <- asks currentModulePath >>= fsReadFile
    ast <- parse src >>= resolveImports
    deps <- M.fromList <$> for (splitImports ast) \(path, modName) -> do
        let context = ModuleContext
                { currentModulePath = path
                , isMainModule = False }
        iMod <- local (const context) compileModule
        pure (modName, iMod)
    core <- runReader deps $ simplify ast
    js <- generate core >>= render
    outputModule js
    pure $ generateInterface core
