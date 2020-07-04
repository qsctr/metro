module Language.Dtfpl.Interface.Parser
    ( parseInterface
    ) where

import           Text.Read

import           Language.Dtfpl.Interface.Syntax

parseInterface :: String -> Maybe IMod
parseInterface = readMaybe
