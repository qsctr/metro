{-# LANGUAGE DataKinds #-}

-- | Functions to make constructing JS syntax trees easier.
module Language.ECMAScript.Syntax.Util
    ( constDecl
    , mkIdentifierE
    ) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err
import           Language.Dtfpl.Err.Util
import           Language.Dtfpl.Generate.GenerateErr
import           Language.ECMAScript.Syntax

-- | @const \<ident> = \<expr>;@
constDecl :: Identifier -> Expression -> Declaration
constDecl ident expr = VariableDeclarationDeclaration $
    VariableDeclaration ConstVariableDeclaration
        [VariableDeclarator (IdentifierPattern ident) (Just expr)]

mkIdentifierE :: Members '[Reader Config, Error Err] r
    => String -> Sem r Identifier
mkIdentifierE s = do
    d <- asks debug
    flip note (mkIdentifier d s) $
        InternalErr $ InternalGenerateErr $ InternalInvalidTargetASTErr $
            errQuote s ++ " is not a valid ECMAScript identifier"
