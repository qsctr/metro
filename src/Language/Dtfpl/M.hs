module Language.Dtfpl.M
    ( module Control.Monad.Except
    , module Control.Monad.Reader
    , M
    , Err (..)
    , Config (..)
    ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Text.Megaparsec.Error

import           Language.Dtfpl.Parse.CustomError

type M = ExceptT Err (Reader Config)

data Err
    = ParsingErr (ParseError Char CustomError)
    deriving Show

data Config = Config
    { debug :: Bool }
