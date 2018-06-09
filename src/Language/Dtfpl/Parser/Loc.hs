{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module Language.Dtfpl.Parser.Loc
    ( Loc (..)
    ) where

import           Data.Data
import           Text.Megaparsec.Pos

data Loc = Loc { start :: SourcePos, end :: SourcePos }
    deriving (Eq, Typeable, Data)

instance Show Loc where
    show Loc {..} = showSourcePos start ++ "-" ++ showSourcePos end
      where showSourcePos SourcePos {..} =
                show (unPos sourceLine) ++ ":" ++ show (unPos sourceColumn)
