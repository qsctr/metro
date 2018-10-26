{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Curry multi-argument lambdas.
module Language.Dtfpl.Simplify.Curry () where

import qualified Data.List.NonEmpty           as N

import           Language.Dtfpl.Simplify.SimM
import           Language.Dtfpl.Step
import           Language.Dtfpl.Syntax

type instance StepClass' 'Curried m = MSim m

-- | Replace all multi-argument lambdas with a chain of single-argument lambdas.
--
-- For example, replaces
--
-- > \x y z -> body
--
-- with
--
-- > \x -> \y -> \z -> body
instance Step Lam 'Curried where
    step (Lam (T idents) expr) = do
        idents' <- traverse step idents
        expr' <- step expr
        pure $ foldr (\i -> Lam i . genLoc . LamExpr)
            (Lam (N.last idents') expr') $ N.init idents'
