{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Simplification phase.
module Language.Metro.Simplify
    ( simplify
    ) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader

import           Language.Metro.Config
import           Language.Metro.Err
import           Language.Metro.Module.Deps
import           Language.Metro.NodeProc
import           Language.Metro.Simplify.GenIdentFull
import           Language.Metro.Step
import           Language.Metro.Syntax

import           Language.Metro.Simplify.AliasCase         ()
import           Language.Metro.Simplify.CheckNativeParams ()
import           Language.Metro.Simplify.Curry             ()
import           Language.Metro.Simplify.InitGen           ()
import           Language.Metro.Simplify.NameResolve
import           Language.Metro.Simplify.ParseNative       ()
import           Language.Metro.Simplify.Reorder           ()
import           Language.Metro.Simplify.UnDef             ()
import           Language.Metro.Simplify.UnLamMatch        ()

-- | Simplify a complete module from ModResolved to Core.
simplify :: Members '[Reader ModuleDeps, Reader Config, Error Err, NodeProc] r
    => A Mod 'ModResolved -> Sem r (A Mod Core)
simplify mod_ = runGenIdentFull $
    step mod_
    >>= step
    >>= step
    >>= step
    >>= step
    >>= step
    >>= nameResolve
    >>= step
    >>= step
