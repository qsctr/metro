{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Functions in the 'M' monad.
module Language.Dtfpl.M.Util
    ( getDebug
    , debugErrIf
    ) where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Language.Dtfpl.Config
import           Language.Dtfpl.Env
import           Language.Dtfpl.Err
import           Language.Dtfpl.M

getDebug :: MEnv m => m Bool
getDebug = asks $ debug . config

-- | Throw an internal error if the condition is true and debug mode is enabled.
debugErrIf :: (MEnv m, MError m) => Bool -> InternalErr -> m ()
debugErrIf cond err = do
    d <- getDebug
    when (d && cond) $ throwError $ InternalErr err
