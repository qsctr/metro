{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Dtfpl.ParseNative
    ( parseNative
    ) where

import           Data.Aeson
import           Polysemy

import           Language.Dtfpl.NodeProc
import           Language.Dtfpl.Step
import           Language.Dtfpl.Syntax
import           Language.ECMAScript.Syntax hiding (Member)

type instance StepEffs 'ParsedNative = '[NodeProc]

data ParseNative
instance Message ParseNative String Value

instance Step Native 'ParsedNative where
    step (Native (P str)) =
        Native . P . PassthruExpression <$> send' @ParseNative str

parseNative :: Member NodeProc r
    => A Prog 'Source -> Sem r (A Prog 'ParsedNative)
parseNative = step
