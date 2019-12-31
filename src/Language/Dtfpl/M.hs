{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}

-- | The core monad 'M' and associated constraints.
module Language.Dtfpl.M
    ( MError
    , MConfig
    , MNodeProc
    , M (..)
    ) where

import           Capability.Error
import           Capability.Reader
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader

import           Language.Dtfpl.Config
import           Language.Dtfpl.Env
import           Language.Dtfpl.Err
import           Language.Dtfpl.NodeProc

-- | Monad that can throw 'Err'.
type MError = HasThrow "err" Err

-- | Monad that can read 'Config'.
type MConfig = HasReader "config" Config

-- | Monad that can access 'NodeProc'.
type MNodeProc = HasReader "nodeProc" NodeProc

type Stack = ExceptT Err (ReaderT Env IO)

-- | The core monad transformer stack.
--
-- - ExceptT for signaling errors
-- - ReaderT for accessing environment (including config)
-- - IO as base monad
newtype M a = M { runM :: Stack a }
    deriving (Functor, Applicative, Monad, MonadIO)
    deriving MError via MonadError Stack
    deriving MConfig via Field "config" () (MonadReader Stack)
    deriving MNodeProc via Field "nodeProc" () (MonadReader Stack)
