-- | Errors for generate phase
module Language.Metro.Generate.GenerateErr
    ( InternalGenerateErr (..)
    ) where

import           Language.Metro.Err.ErrMessage

-- | Internal error during generate phase
data InternalGenerateErr
    -- | The generated JS AST is invalid
    = InternalInvalidTargetASTErr String

instance ErrMessage InternalGenerateErr where
    errMessage (InternalInvalidTargetASTErr msg) =
        ["Attempted to construct invalid ECMAScript AST", msg]
