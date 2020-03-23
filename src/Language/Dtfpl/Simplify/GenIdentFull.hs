{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Language.Dtfpl.Simplify.GenIdentFull
    ( EGenIdentFull
    , genIdentFull
    , genLocIdentFull
    , runGenIdentFull
    ) where

import           Numeric.Natural
import           Polysemy
import           Polysemy.State

import           Language.Dtfpl.Syntax
import           Language.Dtfpl.Syntax.Util

data EGenIdentFull m a where
    GetGenIdentFull
        :: GenIdentFullNum p ~ P Natural => EGenIdentFull m (Ident p)

makeSem ''EGenIdentFull

genIdentFull :: (GenIdentFullNum p ~ P Natural, Member EGenIdentFull r)
    => Sem r (Ident p)
genIdentFull = getGenIdentFull

genLocIdentFull
    :: (GenIdentFullNum p ~ P Natural, Ann p ~ Maybe a, Member EGenIdentFull r)
    => Sem r (A Ident p)
genLocIdentFull = genLoc <$> genIdentFull

runGenIdentFull :: InterpreterFor EGenIdentFull r
runGenIdentFull = evalState 0 . reinterpretAsState
  where reinterpretAsState
            :: Sem (EGenIdentFull ': r) a -> Sem (State Natural ': r) a
        reinterpretAsState = reinterpret $ \case
            GetGenIdentFull -> do
                n <- get
                put $ succ n
                pure $ GenIdentFull $ P n
