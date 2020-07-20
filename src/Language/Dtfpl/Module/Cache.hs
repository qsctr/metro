{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

module Language.Dtfpl.Module.Cache
    ( ModuleCache
    , withModuleCache
    , runModuleCache
    ) where

import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict                  as M
import           Polysemy
import           Polysemy.Reader
import           Polysemy.State
import qualified System.Path                      as P

import           Language.Dtfpl.Interface.Changed
import           Language.Dtfpl.Interface.Syntax
import           Language.Dtfpl.Module.Context
import           Language.Dtfpl.Util.FS

data ModuleCache m a where
    LookupModuleCache :: ModuleCache m (Maybe (IMod, IChanged))
    InsertModuleCache :: (IMod, IChanged) -> ModuleCache m ()

makeSem ''ModuleCache

withModuleCache :: Member ModuleCache r
    => Sem r (IMod, IChanged) -> Sem r (IMod, IChanged)
withModuleCache compile = lookupModuleCache >>= \case
    Just res -> pure res
    Nothing -> do
        res <- compile
        insertModuleCache res
        pure res

runModuleCache :: Members '[Reader ModuleContext, FS] r
    => InterpreterFor ModuleCache r
runModuleCache = evalState @(Map P.AbsFile (IMod, IChanged)) M.empty
    . reinterpret \case
        LookupModuleCache -> getCanonicalizedPath >>= gets . M.lookup
        InsertModuleCache res -> do
            path <- getCanonicalizedPath
            modify' $ M.insert path res
  where getCanonicalizedPath :: Members '[Reader ModuleContext, FS] r
            => Sem r P.AbsFile
        getCanonicalizedPath = asks currentModulePath >>= fsCanonicalizePath
