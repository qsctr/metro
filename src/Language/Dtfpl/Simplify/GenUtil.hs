{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}

-- | Functions for generating nodes in the simplification process.
module Language.Dtfpl.Simplify.GenUtil
    ( genIdentFull
    , genLocIdentFull
    , GenIdentPartT
    , runGenIdentPart
    , genIdentPart
    , genLocIdentPart
    ) where

import Capability.State
import Capability.Reader
import           Data.Singletons.Prelude.Enum
import           Numeric.Natural
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (StateT, evalStateT)

import           Language.Dtfpl.Simplify.SimM
import           Language.Dtfpl.Step
import           Language.Dtfpl.Syntax
import           Language.Dtfpl.Syntax.Util

-- | Returns the next 'GenIdentFull' identifier and updates the 'SimState'.
genIdentFull :: (GenIdentFullNum p ~ P Natural, MSim m) => m (Ident p)
genIdentFull = do
    s@SimState{..} <- get @"sim"
    put @"sim" $ s { nextGenIdentFullNum = succ nextGenIdentFullNum }
    pure $ GenIdentFull $ P nextGenIdentFullNum

-- | Returns an annotated (generated) 'GenIdentFull' identifier and updates the
-- 'SimState'.
genLocIdentFull :: (GenIdentFullNum p ~ P Natural, Ann p ~ Maybe a, MSim m)
    => m (A Ident p)
genLocIdentFull = genLoc <$> genIdentFull

type MGenIdentPartPrefix p = HasReader "prefix" (A Ident p)
type MGenIdentPartNext = HasState "next" Natural
type MGenIdentPart p m = (MGenIdentPartPrefix p m, MGenIdentPartNext m)

type GenIdentPartStack p m = ReaderT (A Ident p) (StateT Natural m)

-- | Monad transformer for generating 'GenIdentPart's.
--
-- - 'ReaderT' for storing the prefix identifier.
-- - 'StateT' for storing the next number.
newtype GenIdentPartT p m a = GenIdentPartT
    { runGenIdentPartT :: GenIdentPartStack p m a }
    deriving (Functor, Applicative, Monad)
    deriving (MGenIdentPartPrefix p) via MonadReader (GenIdentPartStack p m)
    deriving MGenIdentPartNext via MonadState (GenIdentPartStack p m)

-- | Run a 'GenIdentPartM' operation with a prefix for 'GenIdentPart'.
-- This should only be run once for each prefix.
runGenIdentPart :: Monad m => A Ident p -> GenIdentPartT p m a -> m a
runGenIdentPart prefix x = evalStateT (runReaderT (runGenIdentPartT x) prefix) 0

-- | Returns a 'GenIdentPart' and updates the state.
genIdentPart :: (GenIdentPartPrefix p ~ A Ident,
    GenIdentPartNum p ~ P Natural, MGenIdentPart p m) => m (Ident p)
genIdentPart = do
    n <- get @"next"
    put @"next" $ succ n
    prefix <- ask @"prefix"
    pure $ GenIdentPart prefix $ P n

-- | Returns an annotated (generated) 'GenLocIdentPart' and updates the state.
genLocIdentPart :: (GenIdentPartPrefix p ~ A Ident,
    GenIdentPartNum p ~ P Natural, Ann p ~ Maybe a, MGenIdentPart p m)
    => m (A Ident p)
genLocIdentPart = genLoc <$> genIdentPart

-- -- | 'step' lifted to 'GenIdentPartM'.
-- lstep :: (Step n p, StepClass p m) => n (Pred p) -> GenIdentPartT p m (n p)
-- lstep = lift . lift . step
