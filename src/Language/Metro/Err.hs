-- | The main, non phase-specific error types.
--
-- All errors (including internal errors) in the compiler are strongly typed.
--
-- Errors for specifc phases are located within their module hierarchies.
module Language.Metro.Err
    ( Err (..)
    , InternalErr (..)
    ) where

import           Text.Megaparsec.Error

import           Language.Metro.Err.ErrMessage
import           Language.Metro.Generate.GenerateErr
import           Language.Metro.Module.ModuleErr
import           Language.Metro.Parse.CustomError
import           Language.Metro.Simplify.SimplifyErr

-- | Main error type. Represents all errors that the compiler may output.
data Err
    = InternalErr InternalErr
    | ParseErr (ParseErrorBundle String CustomError)
    | ModuleErr ModuleErr
    | SimplifyErr SimplifyErr

-- | Internal errors, i.e. it's not the user's fault.
data InternalErr
    = InternalGenerateErr InternalGenerateErr
    | InternalSimplifyErr InternalSimplifyErr

instance ErrMessage InternalErr where
    errMessage (InternalGenerateErr e) = "Generate error" : errMessage e
    errMessage (InternalSimplifyErr e) = "Simplify error" : errMessage e
