{-# LANGUAGE RecordWildCards #-}

-- | Source locations for annotating nodes.
module Language.Dtfpl.Parser.Loc
    ( Loc (..)
    , formatLoc
    ) where

import           Text.Megaparsec.Pos

-- | Source location of a node.
-- Represented as a range.
data Loc = Loc { start :: SourcePos, end :: SourcePos } deriving Eq

instance Show Loc where show = formatLoc

-- | Display a 'Loc' in a concise readable format
formatLoc :: Loc -> String
formatLoc Loc {..} = formatSourcePos start ++ "-" ++ formatSourcePos end
  where formatSourcePos SourcePos {..} =
            show (unPos sourceLine) ++ ":" ++ show (unPos sourceColumn)
