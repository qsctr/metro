{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}

-- | 'SimM' monad with associated 'SimState' state type.
module Language.Dtfpl.Simplify.SimM
    ( SimState (..)
    , SimT
    , MSim
    , sim
    ) where

import           Capability.State
import           Control.Monad.Trans.State
import           Numeric.Natural

-- | State used in the simplification process.
data SimState = SimState {
    -- | The next number for 'GenIdentFull'.
    -- Fully generated identifiers are numbered in order of node traversal
    -- for the whole program.
    nextGenIdentFullNum :: Natural
} deriving (Eq, Show)

-- | Initial 'SimState' value.
initSimState :: SimState
initSimState = SimState
    { nextGenIdentFullNum = 0 }

type MSim = HasState "sim" SimState

-- | Simplification monad transformer.
-- Uses 'StateT' monad transformer to keep track of simplification state.
newtype SimT m a = SimT { runSimT :: StateT SimState m a }
    deriving (Functor, Applicative, Monad)
    deriving MSim via MonadState (StateT SimState m)

-- | Run a simplification with the initial simplification state.
-- This should only be used on a complete program, not on parts of it,
-- as combining those parts would result in a possibly invalid program.
sim :: Monad m => SimT m a -> m a
sim x = evalStateT (runSimT x) initSimState
