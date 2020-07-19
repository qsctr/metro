module Language.Dtfpl.Interface.Syntax
    ( IMod (..)
    ) where

import           Language.Dtfpl.Syntax

newtype IMod = IMod [IdentBind Core] deriving (Eq, Show, Read)
