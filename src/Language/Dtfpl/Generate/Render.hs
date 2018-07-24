-- | Render a JS AST to JS source code.
-- Just a wrapper around astring library in JavaScript.
module Language.Dtfpl.Generate.Render
    ( render
    ) where

import           Data.Aeson
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe
import           Data.Text                  (Text)
import           System.IO
import           System.Process.Typed

import           Language.ECMAScript.Syntax

-- | Node process running js/main
processConfig :: ProcessConfig Handle Handle ()
processConfig = setStdin createPipe
              $ setStdout createPipe
              $ proc "node" ["js/main"]

-- | Render a JS AST to JS source code.
-- Starts a node process which runs js/main,
-- then sends the JS AST in JSON in estree format to the node script,
-- which uses astring to convert into string.
-- The node process then encodes the resulting string in json and sends it back.
-- Requires IO since it needs to deal with processes.
render :: Program -> IO Text
render program = withProcess processConfig $ \p -> do
    L.hPut (getStdin p) $ encode program `C.snoc` '\n'
    hFlush $ getStdin p
    fromJust . decode . L.fromStrict <$> B.hGetLine (getStdout p)
