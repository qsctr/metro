module Language.ECMAScript.Syntax.Verify
    ( isValidIdentifier
    , isValidIdentifierStart
    , isValidIdentifierPart
    , reservedWords
    ) where

import           Data.Char

isValidIdentifier :: String -> Bool
isValidIdentifier s = and
    [ isValidIdentifierStart (head s)
    , all isValidIdentifierPart (tail s)
    , s `notElem` reservedWords ]

isValidIdentifierStart :: Char -> Bool
isValidIdentifierStart c = or
    [ isLetter c
    , c `elem` "$_"
    , generalCategory c == LetterNumber ]

isValidIdentifierPart :: Char -> Bool
isValidIdentifierPart c = isValidIdentifierStart c
    || generalCategory c `elem`
        [ NonSpacingMark
        , SpacingCombiningMark
        , DecimalNumber
        , ConnectorPunctuation ]

reservedWords :: [String]
reservedWords =
    [ "await"
    , "break"
    , "case", "catch", "class", "const", "continue"
    , "debugger", "default", "delete", "do"
    , "else", "export", "extends"
    , "finally", "for", "function"
    , "if", "import", "in", "instanceof"
    , "new"
    , "return"
    , "super", "switch"
    , "this", "throw", "try", "typeof"
    , "var", "void"
    , "while", "with"
    , "yield" ]
