-- | Functions to verify that the JS AST is valid.
module Language.ECMAScript.Syntax.Verify
    ( isValidIdentifier
    , isValidIdentifierStart
    , isValidIdentifierPart
    , reservedWords
    ) where

import           Data.Char

-- | Returns whether the string is a valid name for a JS identifier.
isValidIdentifier :: String -> Bool
isValidIdentifier s = and
    [ isValidIdentifierStart (head s)
    , all isValidIdentifierPart (tail s)
    , s `notElem` reservedWords ]

-- | Returns if the char is valid as the first char of an identifier.
isValidIdentifierStart :: Char -> Bool
isValidIdentifierStart c = or
    [ isLetter c
    , c `elem` "$_"
    , generalCategory c == LetterNumber ]

-- | Returns if the char is valid as one of the tail chars of an identifier.
isValidIdentifierPart :: Char -> Bool
isValidIdentifierPart c = isValidIdentifierStart c
    || generalCategory c `elem`
        [ NonSpacingMark
        , SpacingCombiningMark
        , DecimalNumber
        , ConnectorPunctuation ]

-- | Strings which cannot be identifier names.
-- Note that in addition to actual reserved keywords this also includes things
-- like @undefined@.
reservedWords :: [String]
reservedWords =
    [ "await"
    , "break"
    , "case", "catch", "class", "const", "continue"
    , "debugger", "default", "delete", "do"
    , "else", "enum", "export", "extends"
    , "false", "finally", "for", "function"
    , "if", "implements", "import", "in", "instanceof", "interface"
    , "let"
    , "new", "null"
    , "package", "private", "protected", "public"
    , "return"
    , "static", "super", "switch"
    , "this", "throw", "true", "try", "typeof"
    , "undefined"
    , "var", "void"
    , "while", "with"
    , "yield" ]
