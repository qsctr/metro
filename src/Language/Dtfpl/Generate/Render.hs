{-# LANGUAGE FlexibleContexts #-}

-- | Render a JS AST to JS source code.
-- Just a wrapper around astring library in JavaScript.
module Language.Dtfpl.Generate.Render
    ( render
    ) where

import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe
import           Data.Text                  (Text)
import           System.IO
import           System.Process.Typed

import           Language.Dtfpl.Env
import           Language.Dtfpl.M
import           Language.ECMAScript.Syntax

-- | Render a JS AST to JS source code.
-- Sends the JS AST in JSON in estree format to the node process,
-- which uses astring to convert into string.
-- The node process then encodes the resulting string in json and sends it back.
-- Requires IO since it needs to deal with processes.
render :: (MEnv m, MonadIO m) => Program -> m Text
render program = do
    p <- asks nodeProc
    liftIO $ L.hPut (getStdin p) $ encode program `C.snoc` '\n'
    liftIO $ hFlush $ getStdin p
    fromJust . decode . L.fromStrict <$> liftIO (B.hGetLine $ getStdout p)
