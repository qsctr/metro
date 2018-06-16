module Main where

import qualified Data.Text.IO       as T
import           Language.Dtfpl
import           System.Environment

main :: IO ()
main = do
    filename <- head <$> getArgs
    program <- readFile $ filename ++ ".dtfpl"
    case compile (Config { debug = True }) program of
        Left err -> putStrLn $ unlines $ showErr err
        Right js -> js >>= T.writeFile (filename ++ ".js")
