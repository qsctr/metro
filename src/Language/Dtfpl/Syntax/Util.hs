{-# LANGUAGE TypeFamilies #-}

module Language.Dtfpl.Syntax.Util
    ( mapNode
    , genLoc
    , absurdP
    ) where

import Language.Dtfpl.Syntax
import Data.Void

-- | Applies a function to the unannotated node inside an annotated node,
-- keeping the same pass.
mapNode :: (n p -> n' p) -> A n p -> A n' p
mapNode f (A n a) = A (f n) a

-- | Annotate a node as a generated node.
genLoc :: Ann p ~ Maybe a => n p -> A n p
genLoc = flip A Nothing

absurdP :: P Void p -> a
absurdP (P x) = absurd x
