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
import           Language.Dtfpl.Syntax.Util

type instance StepEffs 'InitGen = '[]

instance Step n 'InitGen => Step (A n) 'InitGen where
    step (A n a) = flip A (Just a) <$> step n

instance Step Ident 'InitGen where
    step (Ident str)             = pure $ Ident str
    step (GenIdentPart prefix _) = absurdP prefix
    step (GenIdentFull num)      = absurdP num
