{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeFamilies     #-}

module Language.Dtfpl.Module.Cycle
    ( checkImportCycle
    ) where

import           Control.Monad
import           Data.Bool
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as N
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err
import           Language.Dtfpl.Module.Context
import           Language.Dtfpl.Module.ModuleErr
import           Language.Dtfpl.Syntax
import           Language.Dtfpl.Util.FS

checkImportCycle ::
    Members '[Reader ModuleContext, Reader Config, Error Err, FS] r => Sem r ()
checkImportCycle = do
    currPath <- asks currentModulePath
    let findCycle [] = checkPath (throw ()) =<< asks mainModulePath
        findCycle (imp@(A (Import (P path) _) _) : imps) =
            checkPath ((imp :) <$> findCycle imps) path
        checkPath onFalse = (`pathEq` currPath) >=> bool onFalse (pure [])
    either pure throw =<< runError do
        imp :| imps <- note () =<< N.nonEmpty <$> asks importStack
        ModuleErr . CyclicImportErr . (imp :|) <$> findCycle imps
