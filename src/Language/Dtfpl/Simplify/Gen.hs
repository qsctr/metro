{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Language.Dtfpl.Simplify.Gen
    ( genLoc
    , genIdentFull
    , GenIdentPartM
    , runGenIdentPart
    , genIdentPart
    ) where

import           Control.Monad.Reader
import           Control.Monad.State
import           Numeric.Natural

import           Language.Dtfpl.Simplify.Sim
import           Language.Dtfpl.Syntax

genLoc :: Ann p ~ Maybe a => n p -> A n p
genLoc = flip A Nothing

genIdentFull ::
    ( GenIdentFullNum p ~ P Natural
    , Ann p ~ Maybe a )
    => SimM (A Ident p)
genIdentFull = do
    state@SimState{..} <- get
    put $ state { nextGenIdentFullNum = succ nextGenIdentFullNum }
    pure $ genLoc $ GenIdentFull $ P nextGenIdentFullNum

type GenIdentPartM p = ReaderT (A Ident p) (StateT Natural SimM)

runGenIdentPart :: A Ident p -> GenIdentPartM p a -> SimM a
runGenIdentPart prefix x = evalStateT (runReaderT x prefix) 0

genIdentPart ::
    ( GenIdentPartPrefix p ~ A Ident
    , GenIdentPartNum p ~ P Natural
    , Ann p ~ Maybe a )
    => GenIdentPartM p (A Ident p)
genIdentPart = do
    n <- get
    put $ succ n
    prefix <- ask
    pure $ genLoc $ GenIdentPart prefix $ P n
