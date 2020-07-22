{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Dtfpl.Cli.Main
    ( main
    ) where

import           Options.Applicative
import           System.Exit
import           System.IO
import qualified System.Path                as P

import           Language.Dtfpl
import           Language.Dtfpl.Config
import           Language.Dtfpl.Util.CEPath
import           Language.Dtfpl.Util.EPath

data Options = Options
    { file          :: P.AbsRelFile
    , internalDebug :: Bool }

main :: IO ()
main = do
    Options {..} <- execParser opts
    cePath <- mkCEPathIO $ EPath file
    let config = Config
            { debug = internalDebug
            , mainModulePath = cePath
            , moduleSearchPaths = [EPath $ P.takeDirectory file] }
    compile config >>= \case
        Left err -> do
            hPutStr stderr err
            exitFailure
        Right () -> pure ()

opts :: ParserInfo Options
opts = info (options <**> helper) $
    fullDesc
    <> header "dtfpl compiler"

options :: Parser Options
options = do
    file <- fmap P.absRel $ strArgument $
        metavar "FILE"
        <> help "Source file to compile"
    internalDebug <- switch $
        long "internal-debug"
        <> help "Enable internal checks in the compiler"
    pure Options {..}
