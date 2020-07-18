{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Language.Dtfpl.Util.FS
    ( FS
    , fsReadFile
    , fsFileExists
    , fsCanonicalizePath
    , pathEq
    , runFS
    ) where

import           Control.Applicative
import           Data.Function
import           Polysemy

import qualified System.Path               as P
import qualified System.Path.Directory     as PD
import qualified System.Path.IO            as PIO
import qualified System.Path.PartClass     as PC

import           Language.Dtfpl.Util.EPath

data FS m a where
    FsReadFile :: EFile -> FS m String
    FsFileExists :: PC.AbsRel ar => P.File ar -> FS m Bool
    FsCanonicalizePath :: PC.FileDir fd => EPath fd -> FS m (P.Abs fd)

makeSem ''FS

pathEq :: (Member FS r, PC.FileDir fd) => EPath fd -> EPath fd -> Sem r Bool
pathEq = liftA2 (==) `on` fsCanonicalizePath

runFS :: Member (Embed IO) r => InterpreterFor FS r
runFS = interpret \case
    FsReadFile (EPath path) -> embed $ PIO.readFile path
    FsFileExists path -> embed $ PD.doesFileExist path
    FsCanonicalizePath (EPath path) -> embed $ PD.canonicalizePath path
