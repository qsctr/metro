{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Dtfpl.Simplify.GenLoc () where

import           Language.Dtfpl.Simplify.Sim
import           Language.Dtfpl.Syntax

instance Sim n 'GenLoc => Sim (A n) 'GenLoc where
    sim (A n a) = flip A (Just a) <$> sim n
