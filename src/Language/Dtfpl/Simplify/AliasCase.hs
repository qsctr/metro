{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Dtfpl.Simplify.AliasCase () where

import           Data.Traversable

import           Language.Dtfpl.Simplify.GenUtil
import           Language.Dtfpl.Simplify.Sim
import           Language.Dtfpl.Syntax

instance Sim CaseHead 'AliasCase where
    sim (CaseHead (T exprs)) = fmap (CaseHead . T) $
        for exprs $ \expr -> AliExpr <$> sim expr <*> case node expr of
            VarExpr _ -> pure $ T Nothing
            _         -> T . Just <$> genIdentFull
