{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Simplification phase.
module Language.Dtfpl.Simplify
    ( simplify
    ) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err
import           Language.Dtfpl.Module.Deps
import           Language.Dtfpl.NodeProc
import           Language.Dtfpl.Simplify.GenIdentFull
import           Language.Dtfpl.Step
import           Language.Dtfpl.Syntax

import           Language.Dtfpl.Simplify.AliasCase    ()
import           Language.Dtfpl.Simplify.Curry        ()
import           Language.Dtfpl.Simplify.InitGen      ()
import           Language.Dtfpl.Simplify.NameResolve
import           Language.Dtfpl.Simplify.ParseNative  ()
import           Language.Dtfpl.Simplify.Reorder      ()
import           Language.Dtfpl.Simplify.UnDef        ()
import           Language.Dtfpl.Simplify.UnLamMatch   ()

-- | Simplify a complete module from ModResolved to Core.
simplify :: Members '[Reader ModuleDeps, Reader Config, Error Err, NodeProc] r
    => A Mod 'ModResolved -> Sem r (A Mod Core)
simplify mod_ = runGenIdentFull $
    step mod_
    >>= step
    >>= step
    >>= step
    >>= step
    >>= nameResolve
    >>= step
    >>= step
