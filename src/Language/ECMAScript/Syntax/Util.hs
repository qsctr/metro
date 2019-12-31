{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

-- | Functions to make constructing JS syntax trees easier.
module Language.ECMAScript.Syntax.Util
    ( constDecl
    , mkIdentifierM
    ) where

import           Capability.Error
import           Capability.Reader

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err
import           Language.Dtfpl.Err.Util
import           Language.Dtfpl.Generate.ConvertErr
import           Language.Dtfpl.M
import           Language.ECMAScript.Syntax

-- | @const \<ident> = \<expr>;@
constDecl :: Identifier -> Expression -> Declaration
constDecl ident expr = VariableDeclarationDeclaration $
    VariableDeclaration ConstVariableDeclaration
        [VariableDeclarator (IdentifierPattern ident) (Just expr)]

mkIdentifierM :: (MConfig m, MError m) => String -> m Identifier
mkIdentifierM s = do
    d <- asks @"config" debug
    case mkIdentifier d s of
        Just i -> pure i
        Nothing -> throw @"err" $
            InternalErr $ InternalConvertErr $ InternalInvalidTargetASTErr $
                errQuote s ++ " is not a valid ECMAScript identifier"
