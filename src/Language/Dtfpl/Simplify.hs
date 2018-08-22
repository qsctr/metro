{-# LANGUAGE DataKinds #-}

-- | Simplification phase.
module Language.Dtfpl.Simplify
    ( simplify
    ) where

import           Language.Dtfpl.Simplify.Sim
import           Language.Dtfpl.Syntax

import           Language.Dtfpl.Simplify.AliasCase  ()
import           Language.Dtfpl.Simplify.Curry      ()
import           Language.Dtfpl.Simplify.InitGen    ()
import           Language.Dtfpl.Simplify.UnDef      ()
import           Language.Dtfpl.Simplify.UnLamMatch ()

-- | Simplify a complete program from source-form to core-form.
simplify :: A Prog 'Source -> A Prog Core
simplify prog = runSim $ sim prog >>= sim >>= sim >>= sim >>= sim
