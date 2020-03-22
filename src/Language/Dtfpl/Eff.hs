module Language.Dtfpl.Eff
    ( EConfig
    , EErr
    ) where

import           Polysemy.Error
import           Polysemy.Reader

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err

type EConfig = Reader Config

type EErr = Error Err
