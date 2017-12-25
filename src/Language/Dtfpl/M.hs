module Language.Dtfpl.M
    ( M
    ) where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err

type M = ExceptT Err (Reader Config)
