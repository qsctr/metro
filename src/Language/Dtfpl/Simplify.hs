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
import           Language.Dtfpl.Simplify.GenIdentFull
import           Language.Dtfpl.Step
import           Language.Dtfpl.Syntax

import           Language.Dtfpl.Simplify.AliasCase    ()
import           Language.Dtfpl.Simplify.Curry        ()
import           Language.Dtfpl.Simplify.InitGen      ()
import           Language.Dtfpl.Simplify.Reorder      ()
import           Language.Dtfpl.Simplify.Resolve
import           Language.Dtfpl.Simplify.UnDef        ()
import           Language.Dtfpl.Simplify.UnLamMatch   ()

-- | Simplify a complete program from ParsedNative to Core.
simplify :: Members '[Reader Config, Error Err] r
    => A Prog 'ParsedNative -> Sem r (A Prog Core)
simplify prog = runGenIdentFull $
    step prog >>= step >>= step >>= step >>= resolve >>= step >>= step
