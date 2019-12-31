{-# LANGUAGE DeriveGeneric #-}

module Language.Dtfpl.Env
    ( Env (..)
    ) where

import           GHC.Generics

import           Language.Dtfpl.Config
import           Language.Dtfpl.NodeProc

data Env = Env {
    config   :: Config,
    nodeProc :: NodeProc
} deriving Generic
