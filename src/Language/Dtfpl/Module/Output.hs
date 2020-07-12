{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Language.Dtfpl.Module.Output
    ( OutputModule
    , outputModule
    , withOutputPath
    , runOutputModule
    ) where

import           Data.Text                     (Text)
import qualified Data.Text.IO                  as TIO
import           Polysemy
import           Polysemy.Reader
import qualified System.Path                   as P
import qualified System.Path.PartClass         as PC

import           Language.Dtfpl.Module.Context
import           Language.Dtfpl.Util.EPath

data OutputModule m a where
    OutputModule :: Text -> OutputModule m ()

makeSem ''OutputModule

withOutputPath :: EFile -> (forall ar. PC.AbsRel ar => P.File ar -> a) -> a
withOutputPath (EPath path) f = f $ P.replaceExtension path "mjs"

runOutputModule :: Members '[Reader ModuleContext, Embed IO] r
    => InterpreterFor OutputModule r
runOutputModule = interpret \case
    OutputModule js -> do
        path <- asks currentModulePath
        withOutputPath path \outPath ->
            embed $ TIO.writeFile (P.toString outPath) js
