module Language.Dtfpl.Interface.Generate
    ( generateInterface
    ) where

import           Language.Dtfpl.Interface.Syntax
import           Language.Dtfpl.Syntax

generateInterface :: A Mod Core -> IMod
generateInterface (A (Mod _ (T tls)) _) = IMod
    [ bind | A (TLDecl Exp (A (Let bind _) _)) _ <- tls ]
