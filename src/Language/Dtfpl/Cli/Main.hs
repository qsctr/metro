{-# LANGUAGE LambdaCase #-}

module Language.Dtfpl.Cli.Main
    ( main
    ) where

import           System.Environment
import           System.Exit
import           System.IO
import qualified System.Path                as P

import           Language.Dtfpl
import           Language.Dtfpl.Config
import           Language.Dtfpl.Util.CEPath
import           Language.Dtfpl.Util.EPath

main :: IO ()
main = do
    arg <- head <$> getArgs
    let path = P.absRel arg
    cePath <- mkCEPathIO $ EPath path
    let config = Config
            { debug = True
            , mainModulePath = cePath
            , moduleSearchPaths = [EPath $ P.takeDirectory path] }
    compile config >>= \case
        Left err -> do
            hPutStr stderr err
            exitFailure
        Right () -> pure ()
