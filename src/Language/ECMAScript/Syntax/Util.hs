{-# LANGUAGE FlexibleContexts #-}

-- | Functions to make constructing JS syntax trees easier.
module Language.ECMAScript.Syntax.Util
    ( constDecl
    , mkIdentifierM
    ) where

import           Control.Monad.Except

import           Language.Dtfpl.Err
import           Language.Dtfpl.Err.Util
import           Language.Dtfpl.Generate.ConvertErr
import           Language.Dtfpl.M
import           Language.Dtfpl.M.Util
import           Language.ECMAScript.Syntax

-- | @const \<ident> = \<expr>;@
constDecl :: Identifier -> Expression -> Declaration
constDecl ident expr = VariableDeclarationDeclaration $
    VariableDeclaration ConstVariableDeclaration
        [VariableDeclarator (IdentifierPattern ident) (Just expr)]

mkIdentifierM :: (MEnv m, MError m) => String -> m Identifier
mkIdentifierM s = do
    d <- getDebug
    case mkIdentifier d s of
        Just i -> pure i
        Nothing -> throwError $
            InternalErr $ InternalConvertErr $ InternalInvalidTargetASTErr $
                errQuote s ++ " is not a valid ECMAScript identifier"
