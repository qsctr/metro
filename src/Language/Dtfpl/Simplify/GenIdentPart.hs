{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Language.Dtfpl.Simplify.GenIdentPart
    ( EGenIdentPart
    , genIdentPart
    , genLocIdentPart
    , runGenIdentPart
    ) where

import           Numeric.Natural
import           Polysemy
import           Polysemy.State

import           Language.Dtfpl.Syntax
import           Language.Dtfpl.Syntax.Util

type GenIdentPartEnabled p =
    (GenIdentPartPrefix p ~ A Ident, GenIdentPartNum p ~ P Natural)

data EGenIdentPart p m a where
    GetGenIdentPart :: GenIdentPartEnabled p => EGenIdentPart p m (Ident p)

makeSem ''EGenIdentPart

genIdentPart :: (GenIdentPartEnabled p, Member (EGenIdentPart p) r)
    => Sem r (Ident p)
genIdentPart = getGenIdentPart

genLocIdentPart
    :: (GenIdentPartEnabled p, Ann p ~ Maybe a, Member (EGenIdentPart p) r)
    => Sem r (A Ident p)
genLocIdentPart = genLoc <$> genIdentPart

runGenIdentPart :: forall p r. A Ident p -> InterpreterFor (EGenIdentPart p) r
runGenIdentPart prefix = evalState 0 . reinterpretAsState
  where reinterpretAsState
            :: Sem (EGenIdentPart p ': r) a -> Sem (State Natural ': r) a
        reinterpretAsState = reinterpret $ \case
            GetGenIdentPart -> do
                n <- get
                put $ succ n
                pure $ GenIdentPart prefix $ P n
