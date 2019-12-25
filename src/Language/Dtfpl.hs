{-# LANGUAGE NamedFieldPuns #-}

-- | Main dtfpl module.
module Language.Dtfpl
    ( module Language.Dtfpl.Config
    , compile
    ) where

import           Control.Category                ((>>>))
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Bifunctor
import           Data.Text                       (Text)
import           System.Process.Typed

import           Language.Dtfpl.Config
import           Language.Dtfpl.Env
import           Language.Dtfpl.Err.Format
import           Language.Dtfpl.Generate.Convert
import           Language.Dtfpl.Generate.Render
import           Language.Dtfpl.M
import           Language.Dtfpl.NodeProc
import           Language.Dtfpl.ParseNative
import           Language.Dtfpl.Parser
import           Language.Dtfpl.Simplify

-- | Compile a program.
compile :: Config -> String -> IO (Either String Text)
compile config program = withProcessTerm nodeProcConfig $ \nodeProc ->
    fmap (first formatErr) $ flip runReaderT Env { config, nodeProc } $
        runExceptT $ runM $ compileM program

-- | Run the full compilation process in the 'M' monad.
compileM :: String -> M Text
compileM =
    parse ""
    >=> parseNative
    >=> simplify
    >>> convert
    >=> render
