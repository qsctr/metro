{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Language.Metro.Log
    ( Verb
    , LogMsg (..)
    , logMsg
    , runLog
    ) where

import           Colog.Core.Action
import           Colog.Core.IO
import           Colog.Polysemy          as Colog
import           Polysemy
import           Polysemy.Reader

import           Language.Metro.Config
import           Language.Metro.Log.Verb
import           Language.Metro.Util

data LogMsg = LogMsg
    { logMsgVerb :: Verb
    , logMsgStr  :: String }

logMsg :: Member (Log LogMsg) r => Verb -> String -> Sem r ()
logMsg = Colog.log .: LogMsg

formatLogMsg :: LogMsg -> String
formatLogMsg = logMsgStr

runLog :: Members '[Reader Config, Embed IO] r => InterpreterFor (Log LogMsg) r
runLog x = do
    verb <- asks verbosity
    let logAction = cfilter ((<= verb) . logMsgVerb) $
            cmap formatLogMsg logStringStdout
    runLogAction logAction x
