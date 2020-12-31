{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE TupleSections  #-}

module Language.Metro.Module.Compile
    ( compileModule
    ) where

import           Colog.Polysemy
import qualified Data.Map.Strict                   as M
import           Data.Traversable
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader

import           Language.Metro.Config
import           Language.Metro.Err
import           Language.Metro.Generate
import           Language.Metro.Interface.Changed
import           Language.Metro.Interface.Generate
import           Language.Metro.Interface.Syntax
import           Language.Metro.Log
import           Language.Metro.Module.Cache
import           Language.Metro.Module.Context
import           Language.Metro.Module.Cycle
import           Language.Metro.Module.ModFS
import           Language.Metro.Module.Resolve
import           Language.Metro.NodeProc
import           Language.Metro.Parse
import           Language.Metro.Render
import           Language.Metro.Simplify
import           Language.Metro.Syntax.Util
import           Language.Metro.Util.FS

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
            logMsg 1 $ "Compiling " ++ path
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
