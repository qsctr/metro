module Language.Metro.Interface.Syntax
    ( IMod (..)
    ) where

import           Language.Metro.Syntax

newtype IMod = IMod [IdentBind Core] deriving (Eq, Show, Read)
