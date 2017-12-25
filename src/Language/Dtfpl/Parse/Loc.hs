{-# LANGUAGE RecordWildCards #-}

module Language.Dtfpl.Parse.Loc
    ( Loc (..)
    ) where

import Text.Megaparsec.Pos

data Loc = Loc { start :: SourcePos, end :: SourcePos }

instance Show Loc where
    show Loc {..} = showSourcePos start ++ "-" ++ showSourcePos end
      where showSourcePos SourcePos {..} =
                show (unPos sourceLine) ++ ":" ++ show (unPos sourceColumn)
