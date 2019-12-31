{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

-- | Functions in the 'M' monad.
module Language.Dtfpl.M.Util
    ( debugErrIf
    ) where

import           Capability.Error
import           Capability.Reader
import           Control.Monad

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err
import           Language.Dtfpl.M

-- | Throw an internal error if the condition is true and debug mode is enabled.
debugErrIf :: (MConfig m, MError m) => Bool -> InternalErr -> m ()
debugErrIf cond err = do
    d <- asks @"config" debug
    when (d && cond) $ throw @"err" $ InternalErr err
