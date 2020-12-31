{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeFamilies     #-}

module Language.Metro.Module.Cycle
    ( checkImportCycle
    ) where

import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as N
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader

import           Language.Metro.Config
import           Language.Metro.Err
import           Language.Metro.Module.Context
import           Language.Metro.Module.ModuleErr
import           Language.Metro.Syntax
import           Language.Metro.Util.CEPath
import           Language.Metro.Util.FS

checkImportCycle ::
    Members '[Reader ModuleContext, Reader Config, Error Err, FS] r => Sem r ()
checkImportCycle = do
    currPath <- asks $ cPath . currentModulePath
    let findCycle [] = do
            mainPath <- asks $ cPath . mainModulePath
            if mainPath == currPath then pure [] else throw ()
        findCycle (imp@(A (Import (P path) _) _) : imps)
            | cPath path == currPath = pure []
            | otherwise = (imp :) <$> findCycle imps
    either pure throw =<< runError do
        imp :| imps <- note () =<< N.nonEmpty <$> asks importStack
        ModuleErr . CyclicImportErr . (imp :|) <$> findCycle imps
