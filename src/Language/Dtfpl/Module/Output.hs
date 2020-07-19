{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Language.Dtfpl.Module.Output
    ( OutputModule
    , outputModule
    , runOutputModule
    ) where

import           Data.Text                        (Text)
import qualified Data.Text.IO                     as TIO
import           Polysemy
import           Polysemy.Reader
import qualified System.Path                      as P

import           Language.Dtfpl.Module.Context
import           Language.Dtfpl.Module.Extensions
import           Language.Dtfpl.Util.EPath

data OutputModule m a where
    OutputModule :: Text -> OutputModule m ()

makeSem ''OutputModule

runOutputModule :: Members '[Reader ModuleContext, Embed IO] r
    => InterpreterFor OutputModule r
runOutputModule = interpret \case
    OutputModule js -> do
        path <- asks currentModulePath
        withExt outExt path \outPath ->
            embed $ TIO.writeFile (P.toString outPath) js
