{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Dtfpl.ParseNative
    ( parseNative
    ) where

import           Control.Monad.IO.Class
import           Data.Aeson

import           Language.Dtfpl.M
import           Language.Dtfpl.NodeProc.Message
import           Language.Dtfpl.Step
import           Language.Dtfpl.Syntax
import           Language.ECMAScript.Syntax

type instance StepClass' 'ParsedNative m = (MEnv m, MonadIO m)

data ParseNative
instance Message ParseNative String Value

instance Step Native 'ParsedNative where
    step (Native (P str)) =
        Native . P . PassthruExpression <$> send @ParseNative str

parseNative :: (MEnv m, MonadIO m) => A Prog 'Source -> m (A Prog 'ParsedNative)
parseNative = step
