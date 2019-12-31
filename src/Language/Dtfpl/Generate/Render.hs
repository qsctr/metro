{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Language.Dtfpl.Generate.Render
    ( render
    ) where

import           Control.Monad.IO.Class
import           Data.Text                       (Text)

import           Language.Dtfpl.M
import           Language.Dtfpl.NodeProc.Message
import           Language.ECMAScript.Syntax

data Render
instance Message Render Program Text

render :: (MNodeProc m, MonadIO m) => Program -> m Text
render = send @Render
