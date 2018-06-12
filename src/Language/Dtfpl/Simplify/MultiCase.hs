{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Dtfpl.Simplify.MultiCase () where

import           Language.Dtfpl.Simplify.Sim
import           Language.Dtfpl.Syntax

instance Sim CaseHead 'MultiCase where
    sim (CaseHead x) = CaseHead . T . pure <$> sim x

instance Sim CaseAlt 'MultiCase where
    sim (CaseAlt pat expr) = CaseAlt . T . pure <$> sim pat <*> sim expr
