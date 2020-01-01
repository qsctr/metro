{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Simplification phase.
module Language.Dtfpl.Simplify
    ( simplify
    ) where

import           Language.Dtfpl.Simplify.SimM
import           Language.Dtfpl.Step
import           Language.Dtfpl.Syntax
import Language.Dtfpl.M

import           Language.Dtfpl.Simplify.AliasCase  ()
import           Language.Dtfpl.Simplify.Curry      ()
import           Language.Dtfpl.Simplify.InitGen    ()
import           Language.Dtfpl.Simplify.Resolve
import           Language.Dtfpl.Simplify.UnDef      ()
import           Language.Dtfpl.Simplify.UnLamMatch ()

-- | Simplify a complete program from ParsedNative to Core.
simplify :: (MConfig m, MError m) => A Prog 'ParsedNative -> m (A Prog Core)
simplify prog = sim $ step prog >>= step >>= resolve >>= step >>= step >>= step
