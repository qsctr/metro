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

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err.ShowErr
import           Language.Dtfpl.Generate.Convert
import           Language.Dtfpl.Generate.Render
import           Language.Dtfpl.M
import           Language.Dtfpl.Parser
import           Language.Dtfpl.Simplify

-- | Compile a program.
compile :: Config -> String -> Either [String] (IO Text)
compile config = first showErr .
    flip runReader config . runExceptT . runM . compileM

-- | Run the full compilation process in the 'M' monad.
compileM :: String -> M (IO Text)
compileM = parse "" >=> simplify >>> convert >>> fmap render
