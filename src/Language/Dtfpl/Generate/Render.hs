module Language.Dtfpl.Generate.Render
    ( render
    ) where

import           Data.Aeson
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Maybe
import           Data.Text
import           System.IO
import           System.Process.Typed

import           Language.ECMAScript.Syntax

config :: ProcessConfig Handle Handle ()
config = setStdin createPipe
       $ setStdout createPipe
       $ proc "node" ["js/main"]

render :: Program -> IO Text
render program = withProcess config $ \p -> do
    L.hPut (getStdin p) $ encode program `C.snoc` '\n'
    hFlush $ getStdin p
    fromJust . decode . L.fromStrict <$> B.hGetLine (getStdout p)
