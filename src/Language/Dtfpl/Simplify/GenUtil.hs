{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Functions for generating nodes in the simplification process.
module Language.Dtfpl.Simplify.GenUtil
    ( genIdentFull
    , genLocIdentFull
    , GenIdentPartT
    , runGenIdentPart
    , genIdentPart
    , genLocIdentPart
    , lstep
    ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Promotion.Prelude.Enum
import           Numeric.Natural

import           Language.Dtfpl.Simplify.SimM
import           Language.Dtfpl.Step
import           Language.Dtfpl.Syntax

-- | Returns the next 'GenIdentFull' identifier and updates the 'SimState'.
genIdentFull :: (GenIdentFullNum p ~ P Natural, MSim m) => m (Ident p)
genIdentFull = do
    s@SimState{..} <- get
    put $ s { nextGenIdentFullNum = succ nextGenIdentFullNum }
    pure $ GenIdentFull $ P nextGenIdentFullNum

-- | Returns an annotated (generated) 'GenIdentFull' identifier and updates the
-- 'SimState'.
genLocIdentFull :: (GenIdentFullNum p ~ P Natural, Ann p ~ Maybe a, MSim m)
    => m (A Ident p)
genLocIdentFull = genLoc <$> genIdentFull

-- | Monad transformer for generating 'GenIdentPart's.
--
-- - 'ReaderT' for storing the prefix identifier.
-- - 'StateT' for storing the next number.
type GenIdentPartT p m = ReaderT (A Ident p) (StateT Natural m)

-- | Run a 'GenIdentPartM' operation with a prefix for 'GenIdentPart'.
-- This should only be run once for each prefix.
runGenIdentPart :: MSim m => A Ident p -> GenIdentPartT p m a -> m a
runGenIdentPart prefix x = evalStateT (runReaderT x prefix) 0

-- | Returns a 'GenIdentPart' and updates the state.
genIdentPart :: (GenIdentPartPrefix p ~ A Ident,
    GenIdentPartNum p ~ P Natural, Monad m) => GenIdentPartT p m (Ident p)
genIdentPart = do
    n <- get
    put $ succ n
    prefix <- ask
    pure $ GenIdentPart prefix $ P n

-- | Returns an annotated (generated) 'GenLocIdentPart' and updates the state.
genLocIdentPart :: (GenIdentPartPrefix p ~ A Ident,
    GenIdentPartNum p ~ P Natural, Ann p ~ Maybe a, Monad m)
    => GenIdentPartT p m (A Ident p)
genLocIdentPart = genLoc <$> genIdentPart

-- | 'step' lifted to 'GenIdentPartM'.
lstep :: (Step n p, StepClass p m) => n (Pred p) -> GenIdentPartT p m (n p)
lstep = lift . lift . step
