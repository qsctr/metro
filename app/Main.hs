{-# LANGUAGE LambdaCase #-}

module Main where

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
            Right js -> T.writeFile (filename ++ ".mjs") js
