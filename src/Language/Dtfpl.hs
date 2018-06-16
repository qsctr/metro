module Language.Dtfpl
    ( module Language.Dtfpl.Config
    , module Language.Dtfpl.Err
    , module Language.Dtfpl.Err.ShowErr
    , compile
    ) where

import           Control.Category
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Text                       (Text)

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err
import           Language.Dtfpl.Err.ShowErr
import           Language.Dtfpl.Generate.Convert
import           Language.Dtfpl.Generate.Render
import           Language.Dtfpl.M
import           Language.Dtfpl.Parser
import           Language.Dtfpl.Simplify

compile :: Config -> String -> Either Err (IO Text)
compile config program = runReader (runExceptT (compileM program)) config

compileM :: String -> M (IO Text)
compileM = parse "" >=> simplify >>> convert >>> fmap render
