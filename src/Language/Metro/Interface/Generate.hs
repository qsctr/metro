module Language.Metro.Interface.Generate
    ( generateInterface
    ) where

import           Language.Metro.Interface.Syntax
import           Language.Metro.Syntax

generateInterface :: A Mod Core -> IMod
generateInterface (A (Mod _ (T tls)) _) = IMod
    [ bind | A (TLDecl Exp (A (Let bind _) _)) _ <- tls ]
