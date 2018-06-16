module Language.ECMAScript.Syntax.Util
    ( constDecl
    ) where

import Language.ECMAScript.Syntax

constDecl :: Identifier -> Expression -> Declaration
constDecl ident expr = VariableDeclarationDeclaration $
    VariableDeclaration ConstVariableDeclaration
        [VariableDeclarator (IdentifierPattern ident) (Just expr)]
