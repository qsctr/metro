{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Language.Dtfpl.Module.ModFS
    ( ModFS
    , loadSource
    , sourceChanged
    , loadIFile
    , outputIFile
    , updateIFile
    , outputModule
    , runModFS
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.Either
import           Data.Function
import           Data.Functor
import           Data.Text                        (Text)
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader

import           Language.Dtfpl.Err
import           Language.Dtfpl.Interface.Changed
import           Language.Dtfpl.Interface.Parse
import           Language.Dtfpl.Interface.Render
import           Language.Dtfpl.Interface.Syntax
import           Language.Dtfpl.Module.Context
import           Language.Dtfpl.Module.Extensions
import           Language.Dtfpl.Module.ModuleErr
import           Language.Dtfpl.Util.CEPath
import           Language.Dtfpl.Util.EPath
import           Language.Dtfpl.Util.FS

data ModFS m a where
    LoadSource :: ModFS m String
    SourceChanged :: ModFS m Bool
    LoadIFile :: ModFS m (Maybe IMod)
    OutputIFile :: IMod -> ModFS m ()
    UpdateIFile :: IMod -> ModFS m IChanged
    OutputModule :: Text -> ModFS m ()

makeSem ''ModFS

runModFS :: Members '[Reader ModuleContext, Error Err, FS] r
    => InterpreterFor ModFS r
runModFS = interpret \case
    LoadSource -> do
        path <- asks $ ePath . currentModulePath
        fsReadFile path >>= fromEither . first (ModuleErr . LoadModuleErr path)
    SourceChanged -> do
        path <- asks $ ePath . currentModulePath
        fmap (fromRight True) $ runError $
            (liftA2 (>) `on` fromEither <=< fsGetModifyTime) path $
                replaceExt outExt path
    LoadIFile -> getIPath >>= readIFile
    OutputIFile iMod -> getIPath >>= flip writeIFile iMod
    UpdateIFile iMod -> do
        iPath <- getIPath
        readIFile iPath >>= \case
            Just oldIMod
                | oldIMod == iMod -> pure ISame
            _ -> writeIFile iPath iMod $> IChanged
    OutputModule js -> do
        path <- asks $ ePath . currentModulePath
        fsWriteFileText (replaceExt outExt path) js
  where getIPath :: Member (Reader ModuleContext) r => Sem r EFile
        getIPath = replaceExt iExt <$> asks (ePath . currentModulePath)
        readIFile :: Member FS r => EFile -> Sem r (Maybe IMod)
        readIFile = fmap (fromRight Nothing . fmap parseInterface) . fsReadFile
        writeIFile :: Member FS r => EFile -> IMod -> Sem r ()
        writeIFile iPath = fsWriteFile iPath . renderInterface
