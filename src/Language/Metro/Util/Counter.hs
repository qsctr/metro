{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Language.Metro.Util.Counter
    ( Counter
    , next
    , runCounter
    ) where

import           Numeric.Natural
import           Polysemy
import           Polysemy.State

data Counter m a where
    Next :: Counter m Natural

makeSem ''Counter

runCounter :: InterpreterFor Counter r
runCounter = evalState 0 . reinterpretAsState
  where reinterpretAsState :: Sem (Counter ': r) a -> Sem (State Natural ': r) a
        reinterpretAsState = reinterpret $ \case
            Next -> do
                n <- get
                put $ succ n
                pure n
