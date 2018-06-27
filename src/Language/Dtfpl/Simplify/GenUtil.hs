{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Functions for generating nodes in the simplification process.
module Language.Dtfpl.Simplify.GenUtil
    ( genIdentFull
    , genLocIdentFull
    , GenIdentPartM
    , runGenIdentPart
    , genIdentPart
    , genLocIdentPart
    , liftSim
    ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Promotion.Prelude.Enum
import           Numeric.Natural

import           Language.Dtfpl.Simplify.Sim
import           Language.Dtfpl.Syntax

-- | Returns the next 'GenIdentFull' identifier and updates the 'SimState'.
genIdentFull :: GenIdentFullNum p ~ P Natural => SimM (Ident p)
genIdentFull = do
    s@SimState{..} <- get
    put $ s { nextGenIdentFullNum = succ nextGenIdentFullNum }
    pure $ GenIdentFull $ P nextGenIdentFullNum

-- | Returns an annotated (generated) 'GenIdentFull' identifier and updates the
-- 'SimState'.
genLocIdentFull :: (GenIdentFullNum p ~ P Natural, Ann p ~ Maybe a)
    => SimM (A Ident p)
genLocIdentFull = genLoc <$> genIdentFull

-- | Monad for generating 'GenIdentPart's.
--
-- - 'ReaderT' for storing the prefix identifier.
-- - 'StateT' for storing the next number.
--
-- Since this is a monad transformer stack on top of 'SimM', any references to
-- values in the 'SimM' monad should be lifted. In particular, calls to 'sim'
-- can be replaced with 'liftSim'.
type GenIdentPartM p = ReaderT (A Ident p) (StateT Natural SimM)

-- | Run a 'GenIdentPartM' operation with a prefix for 'GenIdentPart'.
-- This should only be run once for each prefix.
runGenIdentPart :: A Ident p -> GenIdentPartM p a -> SimM a
runGenIdentPart prefix x = evalStateT (runReaderT x prefix) 0

-- | Returns a 'GenIdentPart' and updates the state.
genIdentPart :: (GenIdentPartPrefix p ~ A Ident, GenIdentPartNum p ~ P Natural)
    => GenIdentPartM p (Ident p)
genIdentPart = do
    n <- get
    put $ succ n
    prefix <- ask
    pure $ GenIdentPart prefix $ P n

-- | Returns an annotated (generated) 'GenLocIdentPart' and updates the state.
genLocIdentPart :: (GenIdentPartPrefix p ~ A Ident,
    GenIdentPartNum p ~ P Natural, Ann p ~ Maybe a)
    => GenIdentPartM p (A Ident p)
genLocIdentPart = genLoc <$> genIdentPart

-- | 'sim' lifted to 'GenIdentPartM'.
liftSim :: Sim n p => n (Pred p) -> GenIdentPartM p (n p)
liftSim = lift . lift . sim
