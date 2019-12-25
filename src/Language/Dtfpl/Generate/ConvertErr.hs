-- | Errors for convert phase
module Language.Dtfpl.Generate.ConvertErr
    ( InternalConvertErr (..)
    ) where

import           Language.Dtfpl.Err.ErrMessage

-- | Internal error during convert phase
data InternalConvertErr
    -- | The generated JS AST is invalid
    = InternalInvalidTargetASTErr String

instance ErrMessage InternalConvertErr where
    errMessage (InternalInvalidTargetASTErr msg) =
        ["Attempted to construct invalid ECMAScript AST", msg]
