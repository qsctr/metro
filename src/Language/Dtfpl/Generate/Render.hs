{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Language.Dtfpl.Generate.Render
    ( render
    ) where

import           Data.Proxy
import           Data.Text                  (Text)
import           Polysemy

import           Language.Dtfpl.NodeProc
import           Language.ECMAScript.Syntax (Program)

data Render
instance Message Render Program Text

render :: Member Send r => Program -> Sem r Text
render = send $ Proxy @Render
