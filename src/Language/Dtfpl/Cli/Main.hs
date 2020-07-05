{-# LANGUAGE LambdaCase #-}

module Language.Dtfpl.Cli.Main
    ( main
    ) where

import qualified Data.Text.IO       as T
import           Language.Dtfpl
import           System.Environment

main :: IO ()
main = do
    filename <- head <$> getArgs
    readFile (filename ++ ".dtfpl")
        >>= compile (Config { debug = True })
        >>= \case
            Left err -> putStr err
            Right (js, inter) -> do
                T.writeFile (filename ++ ".mjs") js
                writeFile (filename ++ ".dtfpli") inter
