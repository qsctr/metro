{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Add 'Ident' aliases to 'CaseHead's, so they can be cached and referenced in
-- generated code.
module Language.Metro.Simplify.AliasCase () where

import           Data.Traversable

import           Language.Metro.Simplify.GenIdentFull
import           Language.Metro.Step
import           Language.Metro.Syntax

type instance StepEffs 'AliasCase = '[EGenIdentFull]

-- | Attaches an 'Ident' alias to the 'CaseHead' expression if it is not a
-- 'VarExpr'.
instance Step CaseHead 'AliasCase where
    step (CaseHead (T exprs)) = fmap (CaseHead . T) $
        for exprs $ \expr -> AliExpr <$> step expr <*> case node expr of
            VarExpr _ -> pure $ T Nothing
            _         -> T . Just . IdentBind <$> genLocIdentFull
