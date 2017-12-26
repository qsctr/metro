module Language.Dtfpl.Generate.ConvertErr
    ( InternalConvertErr (..)
    ) where

import Language.Dtfpl.Err.ShowErr

data InternalConvertErr
    = InternalInvalidTargetASTErr String

instance ShowErr InternalConvertErr where
    showErr (InternalInvalidTargetASTErr msg) =
        ["Attempted to construct invalid ECMAScript AST", msg]
