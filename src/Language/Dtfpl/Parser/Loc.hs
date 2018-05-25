{-# LANGUAGE RecordWildCards #-}

module Language.Dtfpl.Parser.Loc
    ( Loc (..)
    ) where

import Text.Megaparsec.Pos

data Loc = Loc { start :: SourcePos, end :: SourcePos } deriving Eq

instance Show Loc where
    show Loc {..} = showSourcePos start ++ "-" ++ showSourcePos end
      where showSourcePos SourcePos {..} =
                show (unPos sourceLine) ++ ":" ++ show (unPos sourceColumn)
