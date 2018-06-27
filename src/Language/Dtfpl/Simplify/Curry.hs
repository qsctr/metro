{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Curry multi-argument lambdas.
module Language.Dtfpl.Simplify.Curry () where

import qualified Data.List.NonEmpty              as N

import           Language.Dtfpl.Simplify.Sim
import           Language.Dtfpl.Syntax

-- | Replace all multi-argument lambdas with a chain of single-argument lambdas.
--
-- For example, replaces
--
-- > \x y z -> body
--
-- with
--
-- > \x -> \y -> \z -> body
instance Sim Lam 'Curried where
    sim (Lam (T idents) expr) = do
        idents' <- traverse sim idents
        expr' <- sim expr
        pure $ foldr (\i -> Lam i . genLoc . LamExpr)
            (Lam (N.last idents') expr') $ N.init idents'
