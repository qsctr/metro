{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Main dtfpl module.
module Language.Dtfpl
    ( module Language.Dtfpl.Config
    , compile
    ) where

import           Control.Category                ((>>>))
import           Control.Monad
import           Data.Bifunctor
import           Data.Text                       (Text)
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           Polysemy.Resource

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err
import           Language.Dtfpl.Err.Format
import           Language.Dtfpl.Generate.Convert
import           Language.Dtfpl.Generate.Render
import           Language.Dtfpl.NodeProc
import           Language.Dtfpl.ParseNative
import           Language.Dtfpl.Parser
import           Language.Dtfpl.Simplify

-- | Compile a program.
compile :: Config -> String -> IO (Either String Text)
compile config =
    compileE
    >>> runError
    >>> fmap (first formatErr)
    >>> runReader config
    >>> flip runSend
    >>> withNodeProc
    >>> resourceToIOFinal
    >>> embedToFinal
    >>> runFinal

-- | Run the full compilation process with effects.
compileE :: Members '[Reader Config, Error Err, Send] r => String -> Sem r Text
compileE =
    parse ""
    >=> parseNative
    >=> simplify
    >=> convert
    >=> render
