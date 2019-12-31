{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}

module Language.Dtfpl.NodeProc.Message
    ( Message
    , send
    ) where

import           Capability.Reader
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe
import           Data.Typeable
import           System.IO
import           System.Process.Typed

import           Language.Dtfpl.M

class (Typeable t, ToJSON req, FromJSON res)
    => Message t req res | t -> req res where

send :: forall t req res m.
    (Message t req res, MNodeProc m, MonadIO m) => req -> m res
send x = do
    p <- ask @"nodeProc"
    let req = object
            [ "type" .= show (typeRep $ Proxy @t)
            , "value" .= x ]
    liftIO $ do
        L.hPut (getStdin p) $ encode req `C.snoc` '\n'
        hFlush $ getStdin p
        fromJust . decode . L.fromStrict <$> B.hGetLine (getStdout p)
