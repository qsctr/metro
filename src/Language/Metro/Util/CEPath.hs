{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeFamilies     #-}

module Language.Metro.Util.CEPath
    ( CEPath
    , cPath
    , ePath
    , CEFile
    , CEDir
    , mkCEPath
    , mkCEPathIO
    ) where

import           Polysemy
import qualified System.Path               as P
import qualified System.Path.Part          as PP
import qualified System.Path.PartClass     as PC

import           Language.Metro.Util.EPath
import           Language.Metro.Util.FS

data CEPath fd = CEPath { cPath :: P.Abs fd, ePath :: EPath fd }

type CEFile = CEPath PP.File
type CEDir = CEPath PP.Dir

mkCEPath :: (Member FS r, PC.FileDir fd) => EPath fd -> Sem r (CEPath fd)
mkCEPath ePath = do
    cPath <- fsCanonicalizePath ePath
    pure CEPath {..}

mkCEPathIO :: PC.FileDir fd => EPath fd -> IO (CEPath fd)
mkCEPathIO = runM . runFS . mkCEPath
