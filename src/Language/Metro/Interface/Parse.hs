module Language.Metro.Interface.Parse
    ( parseInterface
    ) where

import           Text.Read

import           Language.Metro.Interface.Syntax

parseInterface :: String -> Maybe IMod
parseInterface = readMaybe
