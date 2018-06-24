{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The core monad 'M' and associated constraints.
module Language.Dtfpl.M
    ( MError
    , MConfig
    , M (..)
    ) where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err

-- | Monad that can throw 'Err'.
type MError = MonadError Err

-- | Monad that can read 'Config'.
type MConfig = MonadReader Config

-- | The core monad transformer stack.
-- 
-- - ExceptT for signaling errors
-- - Reader for accessing config
newtype M a = M { runM :: ExceptT Err (Reader Config) a }
    deriving (Functor, Applicative, Monad, MError, MConfig)
