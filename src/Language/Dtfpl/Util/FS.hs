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
    , fsWriteFile
    , fsWriteFileText
    , fsFileExists
    , fsGetModifyTime
    , fsCanonicalizePath
    , runFS
    ) where

import           Control.Exception
import           Data.Text                 (Text)
import qualified Data.Text.IO              as TIO
import           Data.Time.Clock
import           Polysemy
import           System.IO.Error
import qualified System.Path               as P
import qualified System.Path.Directory     as PD
import qualified System.Path.IO            as PIO
import qualified System.Path.PartClass     as PC

import           Language.Dtfpl.Util.EPath

data FS m a where
    FsReadFile :: EFile -> FS m (Either IOError String)
    FsWriteFile :: EFile -> String -> FS m ()
    FsWriteFileText :: EFile -> Text -> FS m ()
    FsFileExists :: PC.AbsRel ar => P.File ar -> FS m Bool
    FsGetModifyTime :: PC.FileDir fd =>
        EPath fd -> FS m (Either IOError UTCTime)
    FsCanonicalizePath :: PC.FileDir fd => EPath fd -> FS m (P.Abs fd)

makeSem ''FS

runFS :: Member (Embed IO) r => InterpreterFor FS r
runFS = interpret \case
    FsReadFile (EPath path) ->
        embed $ catchReadErrors $ PIO.readFile path
    FsWriteFile (EPath path) contents ->
        embed $ PIO.writeFile path contents
    FsWriteFileText (EPath path) contents ->
        embed $ TIO.writeFile (P.toString path) contents
    FsFileExists path ->
        embed $ PD.doesFileExist path
    FsGetModifyTime (EPath path) ->
        embed $ catchReadErrors $ PD.getModificationTime path
    FsCanonicalizePath (EPath path) ->
        embed $ PD.canonicalizePath path
  where catchReadErrors = tryJust \e ->
            if isDoesNotExistError e || isPermissionError e
                then Just e
                else Nothing
