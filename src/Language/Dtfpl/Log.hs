{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Language.Dtfpl.Log
    ( Verb (..)
    , LogMsg (..)
    , formatLogMsg
    , logMsg
    ) where

import           Colog.Polysemy      as Colog
import           Polysemy

import           Language.Dtfpl.Util

data Verb = V0 | V1

data LogMsg = LogMsg
    { logMsgVerb :: Verb
    , logMsgStr  :: String }

formatLogMsg :: LogMsg -> String
formatLogMsg = logMsgStr

logMsg :: Member (Log LogMsg) r => Verb -> String -> Sem r ()
logMsg = Colog.log .: LogMsg
