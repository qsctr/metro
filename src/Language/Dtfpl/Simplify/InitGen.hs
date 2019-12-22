{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Initial generated AST pass.
--
-- Enables the constructors for generated nodes which were disabled for 'Source'
-- pass.
module Language.Dtfpl.Simplify.InitGen () where

import           Language.Dtfpl.Step
import           Language.Dtfpl.Syntax

type instance StepClass' 'InitGen m = ()

instance Step n 'InitGen => Step (A n) 'InitGen where
    step (A n a) = flip A (Just a) <$> step n

instance Step Ident 'InitGen where
    step (Ident str)        = pure $ Ident str
    step (GenIdentPart _ _) = undefined
    step (GenIdentFull _)   = undefined
