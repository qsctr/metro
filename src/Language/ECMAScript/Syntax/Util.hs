{-# LANGUAGE DataKinds        #-}

-- | Functions to make constructing JS syntax trees easier.
module Language.ECMAScript.Syntax.Util
    ( constDecl
    , mkIdentifierE
    ) where

import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader

import           Language.Dtfpl.Config
import           Language.Dtfpl.Eff
import           Language.Dtfpl.Err
import           Language.Dtfpl.Err.Util
import           Language.Dtfpl.Generate.ConvertErr
import           Language.ECMAScript.Syntax

-- | @const \<ident> = \<expr>;@
constDecl :: Identifier -> Expression -> Declaration
constDecl ident expr = VariableDeclarationDeclaration $
    VariableDeclaration ConstVariableDeclaration
        [VariableDeclarator (IdentifierPattern ident) (Just expr)]

mkIdentifierE :: Members '[EConfig, EErr] r => String -> Sem r Identifier
mkIdentifierE s = do
    d <- asks debug
    case mkIdentifier d s of
        Just i -> pure i
        Nothing -> throw $
            InternalErr $ InternalConvertErr $ InternalInvalidTargetASTErr $
                errQuote s ++ " is not a valid ECMAScript identifier"
