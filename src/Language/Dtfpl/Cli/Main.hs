{-# LANGUAGE LambdaCase #-}

module Language.Dtfpl.Cli.Main
    ( main
    ) where

import           System.Environment
import qualified System.Path               as P

import           Language.Dtfpl
import           Language.Dtfpl.Config
import           Language.Dtfpl.Util.EPath

main :: IO ()
main = do
    arg <- head <$> getArgs
    let path = P.absRel arg
        config = Config
            { debug = True
            , moduleSearchPaths = [EPath $ P.takeDirectory path] }
    compile config (EPath path) >>= \case
        Left err -> putStr err
        Right () -> pure ()
