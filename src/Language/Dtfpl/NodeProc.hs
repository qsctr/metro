{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

module Language.Dtfpl.NodeProc
    ( Message
    , Send
    , send'
    , NodeProc
    , runSend
    , withNodeProc
    ) where

import           Data.Aeson
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe
import           Data.Typeable
import           Polysemy
import           Polysemy.Resource
import           System.IO
import           System.Process.Typed

class (Typeable t, ToJSON req, FromJSON res) => Message t req res | t -> req res

data Send m a where
    Send :: Message t req res => Proxy t -> req -> Send m res

makeSem ''Send

send' :: forall t req res r.
    (Message t req res, Member Send r) => req -> Sem r res
send' = send $ Proxy @t

type NodeProc = Process Handle Handle ()

runSend :: Member (Embed IO) r => NodeProc -> InterpreterFor Send r
runSend p = interpret $ \case
    Send proxy x -> do
        let req = object
                [ "type" .= show (typeRep proxy)
                , "value" .= x ]
        embed $ do
            L.hPut (getStdin p) $ encode req `C.snoc` '\n'
            hFlush $ getStdin p
            fromJust . decode . L.fromStrict <$> B.hGetLine (getStdout p)

withNodeProc :: Members '[Resource, Embed IO] r
    => (NodeProc -> Sem r a) -> Sem r a
withNodeProc = bracket (startProcess config) stopProcess
  where config = setStdin createPipe
               $ setStdout createPipe
               $ proc "node" ["js/main"]
