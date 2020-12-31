{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Language.Metro.Simplify.GenIdentFull
    ( EGenIdentFull
    , genIdentFull
    , genLocIdentFull
    , runGenIdentFull
    ) where

import           Numeric.Natural
import           Polysemy

import           Language.Metro.Syntax
import           Language.Metro.Syntax.Util
import           Language.Metro.Util.Counter

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
runGenIdentFull = runCounter . reinterpret \case
    GetGenIdentFull -> GenIdentFull . P <$> next
