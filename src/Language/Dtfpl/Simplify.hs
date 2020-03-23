{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Simplification phase.
module Language.Dtfpl.Simplify
    ( simplify
    ) where

import           Polysemy

import           Language.Dtfpl.Simplify.GenIdentFull
import           Language.Dtfpl.Step
import           Language.Dtfpl.Syntax

import           Language.Dtfpl.Simplify.AliasCase    ()
import           Language.Dtfpl.Simplify.Curry        ()
import           Language.Dtfpl.Simplify.InitGen      ()
import           Language.Dtfpl.Simplify.Resolve
import           Language.Dtfpl.Simplify.UnDef        ()
import           Language.Dtfpl.Simplify.UnLamMatch   ()

-- | Simplify a complete program from ParsedNative to Core.
simplify :: A Prog 'ParsedNative -> Sem r (A Prog Core)
simplify prog = runGenIdentFull $ step prog >>= step >>= resolve >>= step >>= step >>= step
