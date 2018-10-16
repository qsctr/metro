{-# LANGUAGE ConstraintKinds #-}

-- | 'SimM' monad with associated 'SimState' state type.
module Language.Dtfpl.Simplify.SimM
    ( SimState (..)
    , SimM
    , MSim
    , runSim
    ) where

import           Control.Monad.State
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

-- | Simplification monad.
-- Uses 'State' monad to keep track of simplification state.
type SimM = State SimState

type MSim = MonadState SimState

-- | Run a simplification with the initial simplification state.
-- This should only be used on a complete program, not on parts of it,
-- as combining those parts would result in a possibly invalid program.
runSim :: SimM a -> a
runSim = flip evalState initSimState
