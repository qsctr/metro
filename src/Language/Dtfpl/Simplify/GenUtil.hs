{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

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

genIdentFull :: GenIdentFullNum p ~ P Natural => SimM (Ident p)
genIdentFull = do
    s@SimState{..} <- get
    put $ s { nextGenIdentFullNum = succ nextGenIdentFullNum }
    pure $ GenIdentFull $ P nextGenIdentFullNum

genLocIdentFull :: (GenIdentFullNum p ~ P Natural, Ann p ~ Maybe a)
    => SimM (A Ident p)
genLocIdentFull = genLoc <$> genIdentFull

type GenIdentPartM p = ReaderT (A Ident p) (StateT Natural SimM)

runGenIdentPart :: A Ident p -> GenIdentPartM p a -> SimM a
runGenIdentPart prefix x = evalStateT (runReaderT x prefix) 0

genIdentPart :: (GenIdentPartPrefix p ~ A Ident, GenIdentPartNum p ~ P Natural)
    => GenIdentPartM p (Ident p)
genIdentPart = do
    n <- get
    put $ succ n
    prefix <- ask
    pure $ GenIdentPart prefix $ P n

genLocIdentPart :: (GenIdentPartPrefix p ~ A Ident,
    GenIdentPartNum p ~ P Natural, Ann p ~ Maybe a)
    => GenIdentPartM p (A Ident p)
genLocIdentPart = genLoc <$> genIdentPart

liftSim :: Sim n p => n (Pred p) -> GenIdentPartM p (n p)
liftSim = lift . lift . sim
