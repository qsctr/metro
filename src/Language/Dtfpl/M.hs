module Language.Dtfpl.M
    ( M
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
