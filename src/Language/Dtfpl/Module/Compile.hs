{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TupleSections  #-}

module Language.Dtfpl.Module.Compile
    ( compileModule
    ) where

import           Colog.Polysemy
import qualified Data.Map.Strict                   as M
import           Data.Traversable
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err
import           Language.Dtfpl.Generate
import           Language.Dtfpl.Interface.Changed
import           Language.Dtfpl.Interface.Generate
import           Language.Dtfpl.Interface.Syntax
import           Language.Dtfpl.Log
import           Language.Dtfpl.Module.Cache
import           Language.Dtfpl.Module.Context
import           Language.Dtfpl.Module.Cycle
import           Language.Dtfpl.Module.ModFS
import           Language.Dtfpl.Module.Resolve
import           Language.Dtfpl.NodeProc
import           Language.Dtfpl.Parse
import           Language.Dtfpl.Render
import           Language.Dtfpl.Simplify
import           Language.Dtfpl.Syntax.Util
import           Language.Dtfpl.Util.FS

compileModule :: Members '[ModFS, ModuleCache, Reader ModuleContext,
    Reader Config, Error Err, NodeProc, FS, Log LogMsg] r
    => Sem r (IMod, IChanged)
compileModule = withModuleCache do
    checkImportCycle
    src <- loadSource
    ast <- parse src >>= resolveImports
    (deps, iChangeds) <- unzip <$> for (getImports ast) \imp -> do
        let (path, modName) = splitImport imp
            newContext ctx = ModuleContext
                { currentModulePath = path
                , importStack = imp : importStack ctx }
        (iMod, iChanged) <- local newContext compileModule
        pure ((modName, iMod), iChanged)
    let compile = do
            path <- asks currentModulePathString
            logMsg V1 $ "Compiling " ++ path
            core <- runReader (M.fromList deps) $ simplify ast
            js <- generate core >>= render
            outputModule js
            pure $ generateInterface core
    srcChanged <- sourceChanged
    if not srcChanged && all (== ISame) iChangeds
        then loadIFile >>= \case
            Just iMod -> pure (iMod, ISame)
            Nothing -> do
                iMod <- compile
                outputIFile iMod
                pure (iMod, IChanged)
        else do
            iMod <- compile
            (iMod, ) <$> updateIFile iMod
