-- | The main, non phase-specific error types.
--
-- All errors (including internal errors) in the compiler are strongly typed.
--
-- Errors for specifc phases are located within their module hierarchies.
module Language.Dtfpl.Err
    ( Err (..)
    , InternalErr (..)
    , errQuote
    ) where

import           Text.Megaparsec.Error

import           Language.Dtfpl.Err.ErrMessage
import           Language.Dtfpl.Generate.ConvertErr
import           Language.Dtfpl.Parser.CustomError

-- | Main error type. Represents all errors that the compiler may output.
data Err
    = InternalErr InternalErr
    | ParseErr (ParseErrorBundle String CustomError)

-- | Internal errors, i.e. it's not the user's fault.
data InternalErr
    = InternalConvertErr InternalConvertErr

instance ErrMessage InternalErr where
    errMessage (InternalConvertErr e) = "Convert error" : errMessage e

-- | Wrap a string with backticks.
-- Useful for showing pieces of code in error messages.
errQuote :: String -> String
errQuote = ('`' :) . (++ "`")
