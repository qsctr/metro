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

import           Data.Text                     (Text)
import qualified Data.Text.IO                  as TIO
import           Polysemy
import           Polysemy.Reader
import qualified System.Path                   as P

import           Language.Dtfpl.Module.Context
import           Language.Dtfpl.Util.EPath

data OutputModule m a where
    OutputModule :: Text -> OutputModule m ()

makeSem ''OutputModule

runOutputModule :: Members '[Reader ModuleContext, Embed IO] r
    => InterpreterFor OutputModule r
runOutputModule = interpret \case
    OutputModule js -> do
        EPath path <- asks currentModulePath
        let outFile = P.toString $ P.replaceExtension path "mjs"
        embed $ TIO.writeFile outFile js
