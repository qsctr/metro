module Language.Dtfpl.M
    ( module Control.Monad.Except
    , module Control.Monad.Reader
    , module Language.Dtfpl.Config
    , module Language.Dtfpl.Err
    , M
    , debugErrIf
    ) where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err

type M = ExceptT Err (Reader Config)

debugErrIf :: Bool -> InternalErr -> M ()
debugErrIf cond err = do
    d <- asks debug
    when (d && cond) $ throwError $ InternalErr err
