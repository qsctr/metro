{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Dtfpl.Simplify.Curry () where

import qualified Data.List.NonEmpty              as N

import           Language.Dtfpl.Simplify.GenUtil
import           Language.Dtfpl.Simplify.Sim
import           Language.Dtfpl.Syntax

instance Sim Lam 'Curried where
    sim (Lam (T idents) expr) = do
        idents' <- traverse sim idents
        expr' <- sim expr
        pure $ foldr (\i -> Lam i . genLoc . LamExpr)
            (Lam (N.last idents') expr') $ N.init idents'
