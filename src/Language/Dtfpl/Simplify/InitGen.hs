{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Initial generated AST pass.
--
-- Enables the constructors for generated nodes which were disabled for 'Source'
-- pass.
module Language.Dtfpl.Simplify.InitGen () where

import           Language.Dtfpl.Simplify.Sim
import           Language.Dtfpl.Syntax

instance Sim n 'InitGen => Sim (A n) 'InitGen where
    sim (A n a) = flip A (Just a) <$> sim n

instance Sim Ident 'InitGen where
    sim (Ident str)        = pure $ Ident str
    sim (GenIdentPart _ _) = undefined
    sim (GenIdentFull _)   = undefined
