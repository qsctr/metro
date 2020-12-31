{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Language.Metro.Simplify.GenIdentPart
    ( EGenIdentPart
    , genIdentPart
    , genLocIdentPart
    , runGenIdentPart
    ) where

import           Numeric.Natural
import           Polysemy

import           Language.Metro.Syntax
import           Language.Metro.Syntax.Util
import           Language.Metro.Util.Counter

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

runGenIdentPart :: A Ident p -> InterpreterFor (EGenIdentPart p) r
runGenIdentPart prefix = runCounter . reinterpret (\case
    GetGenIdentPart -> GenIdentPart prefix . P <$> next)
