{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Main dtfpl module.
module Language.Dtfpl
    ( module Language.Dtfpl.Config
    , compile
    ) where

import           Control.Category                  ((>>>))
import           Data.Bifunctor
import           Data.Function
import           Data.Text                         (Text)
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           Polysemy.Resource

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err
import           Language.Dtfpl.Err.Format
import           Language.Dtfpl.Generate
import           Language.Dtfpl.Interface.Generate
import           Language.Dtfpl.Interface.Render
import           Language.Dtfpl.NodeProc
import           Language.Dtfpl.Parse
import           Language.Dtfpl.ParseNative
import           Language.Dtfpl.Render
import           Language.Dtfpl.Simplify

-- | Compile a module.
compile :: Config -> String -> IO (Either String (Text, String))
compile config =
    compileE
    >>> runError
    >>> fmap (first formatErr)
    >>> runReader config
    >>> runNodeProc
    >>> resourceToIOFinal
    >>> embedToFinal
    >>> runFinal

-- | Run the full compilation process with effects.
compileE :: Members '[Reader Config, Error Err, NodeProc] r
    => String -> Sem r (Text, String)
compileE src = do
    core <- parse "" src >>= parseNative >>= simplify
    js <- generate core >>= render
    let inter = generateInterface core & renderInterface
    pure (js, inter)
