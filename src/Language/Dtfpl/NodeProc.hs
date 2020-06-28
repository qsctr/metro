{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BlockArguments         #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

module Language.Dtfpl.NodeProc
    ( Message
    , NodeProc
    , send'
    , runNodeProc
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

import           Paths_dtfpl

class (Typeable t, ToJSON req, FromJSON res) => Message t req res | t -> req res

data NodeProc m a where
    Send :: Message t req res => Proxy t -> req -> NodeProc m res

makeSem ''NodeProc

send' :: forall t req res r.
    (Message t req res, Member NodeProc r) => req -> Sem r res
send' = send $ Proxy @t

runNodeProc :: Members '[Resource, Embed IO] r => InterpreterFor NodeProc r
runNodeProc a = do
    jsMain <- embed $ getDataFileName "js/main.js"
    let config = setStdin createPipe
               $ setStdout createPipe
               $ proc "node" [jsMain]
    bracket (startProcess config) stopProcess \p -> interpret
        \case
            Send proxy x -> embed do
                let req = object
                        [ "type" .= show (typeRep proxy)
                        , "value" .= x ]
                L.hPut (getStdin p) $ encode req `C.snoc` '\n'
                hFlush $ getStdin p
                fromJust . decode . L.fromStrict <$> B.hGetLine (getStdout p)
        a
