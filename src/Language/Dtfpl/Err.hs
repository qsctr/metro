{-# LANGUAGE FlexibleInstances #-}

module Language.Dtfpl.Err
    ( Err (..)
    , InternalErr (..)
    , errQuote
    ) where

import           Text.Megaparsec.Error

import           Language.Dtfpl.Err.ShowErr
import           Language.Dtfpl.Generate.ConvertErr
import           Language.Dtfpl.Parse.CustomError

data Err
    = InternalErr InternalErr
    | ParseErr (ParseError Char CustomError)

instance ShowErr Err where
    showErr (InternalErr e) = "Internal error" : showErr e
    showErr (ParseErr e)    = "Parse error" : showErr e

data InternalErr
    = InternalConvertErr InternalConvertErr

instance ShowErr InternalErr where
    showErr (InternalConvertErr e) = "Convert error" : showErr e

errQuote :: String -> String
errQuote = ('`' :) . (++ "`")
