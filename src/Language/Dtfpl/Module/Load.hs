{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Language.Dtfpl.Module.Load
    ( LoadModule
    , loadModule
    , moduleExists
    , runLoadModule
    ) where

import           Polysemy
import           Polysemy.Reader
import qualified System.Path                   as P
import qualified System.Path.Directory         as PD
import qualified System.Path.IO                as PIO
import qualified System.Path.PartClass         as PC

import           Language.Dtfpl.Module.Context
import           Language.Dtfpl.Util.EPath

data LoadModule m a where
    LoadModule :: LoadModule m String
    ModuleExists :: PC.AbsRel ar => P.File ar -> LoadModule m Bool

makeSem ''LoadModule

runLoadModule :: Members '[Reader ModuleContext, Embed IO] r
    => InterpreterFor LoadModule r
runLoadModule = interpret \case
    LoadModule -> do
        EPath path <- asks currentModulePath
        embed $ PIO.readFile path
    ModuleExists path -> do
        embed $ PD.doesFileExist path
