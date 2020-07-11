{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Dtfpl.Format
    ( Format (..)
    ) where

import           Data.List
import qualified Data.List.NonEmpty         as N
import           Data.Void
import           Numeric.Natural
import           Text.Megaparsec.Pos

import           Language.Dtfpl.Parse.Loc
import           Language.Dtfpl.Syntax
import           Language.Dtfpl.Syntax.Util

-- | AST nodes that can be formatted
class Format a where
    -- | Format an AST node into a string, e.g. for displaying in error messages
    format :: a -> String

instance Format Loc where
    format Loc {..} = sourceName start ++ " "
        ++ formatSourcePos start ++ "-" ++ formatSourcePos end
      where formatSourcePos SourcePos {..} =
                show (unPos sourceLine) ++ ":" ++ show (unPos sourceColumn)

instance Format t => Format (P t p) where
    format (P x) = format x

instance Format (n p) => Format (A n p) where
    format (A n _) = format n

instance Format ModName where
    format (ModName atoms) = intercalate "." $ map format $ N.toList atoms

instance Format ModAtom where
    format (ModAtom str) = str

instance Format (Ident p) => Format (IdentBind p) where
    format (IdentBind ident) = format ident

class FormatIdent genPartPrefix genPartNum genFullNum where
    formatIdent :: (GenIdentPartPrefix p ~ genPartPrefix,
                    GenIdentPartNum p ~ genPartNum,
                    GenIdentFullNum p ~ genFullNum)
                   => Ident p -> String

instance FormatIdent (P Void) (P Void) (P Void) where
    formatIdent (Ident s)               = s
    formatIdent (GenIdentPart prefix _) = absurdP prefix
    formatIdent (GenIdentFull num)      = absurdP num

instance FormatIdent (A Ident) (P Natural) (P Natural) where
    formatIdent (Ident s) = s
    formatIdent (GenIdentPart prefix (P num)) =
        "<GEN " ++ format prefix ++ " " ++ show num ++ ">"
    formatIdent (GenIdentFull (P num)) = "<GEN " ++ show num ++ ">"

instance FormatIdent (GenIdentPartPrefix p) (GenIdentPartNum p)
                     (GenIdentFullNum p)
         => Format (Ident p) where
    format = formatIdent
