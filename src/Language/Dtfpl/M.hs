-- | The core monad 'M' and some related functions.
module Language.Dtfpl.M
    ( M
    , debugErrIf
    ) where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err

-- | The core monad transformer stack.
-- 
-- - ExceptT for signaling errors
-- - Reader for accessing config
type M = ExceptT Err (Reader Config)

-- | Throw an internal error if the condition is true and debug mode is enabled.
debugErrIf :: Bool -> InternalErr -> M ()
debugErrIf cond err = do
    d <- asks debug
    when (d && cond) $ throwError $ InternalErr err
