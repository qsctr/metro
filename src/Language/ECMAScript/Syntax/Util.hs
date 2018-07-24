-- | Functions to make constructing JS syntax trees easier.
module Language.ECMAScript.Syntax.Util
    ( constDecl
    ) where

import Language.ECMAScript.Syntax

-- | @const \<ident> = \<expr>;@
constDecl :: Identifier -> Expression -> Declaration
constDecl ident expr = VariableDeclarationDeclaration $
    VariableDeclaration ConstVariableDeclaration
        [VariableDeclarator (IdentifierPattern ident) (Just expr)]
