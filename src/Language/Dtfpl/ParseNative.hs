{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Dtfpl.ParseNative
    ( parseNative
    ) where

import           Control.Monad.IO.Class

import           Language.Dtfpl.Step
import           Language.Dtfpl.Syntax
import           Language.ECMAScript.Syntax

type instance StepClass' 'ParsedNative = MonadIO

instance Step Native 'ParsedNative where
    step (Native _) = do
        liftIO $ putStrLn "hello"
        pure $ Native $ P ThisExpression

parseNative :: A Prog 'Source -> IO (A Prog 'ParsedNative)
parseNative = step
