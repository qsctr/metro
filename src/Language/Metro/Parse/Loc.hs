{-# LANGUAGE DeriveDataTypeable #-}

-- | Source locations for annotating nodes.
module Language.Metro.Parse.Loc
    ( Loc (..)
    ) where

import           Data.Data
import           Text.Megaparsec.Pos

-- | Source location of a node.
-- Represented as a range.
data Loc = Loc { start :: SourcePos, end :: SourcePos }
    deriving (Eq, Ord, Show, Read, Data)
