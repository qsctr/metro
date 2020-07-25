module Language.Dtfpl.Generate.Identifier
    ( identifierize
    ) where

import           Language.ECMAScript.Syntax.Verify

-- | Turn a string into a string that would be a valid JS identifier.
identifierize :: String -> String
identifierize = concatMap convertChar
  where convertChar '-' = "_"
        convertChar '_' = "$u$"
        convertChar '$' = "$d$"
        convertChar c
            | isValidIdentifierPart c = [c]
            | otherwise = "$c" ++ show (fromEnum c) ++ "$"
