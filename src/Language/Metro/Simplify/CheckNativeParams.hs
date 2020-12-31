{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Metro.Simplify.CheckNativeParams () where

import           Data.Foldable
import           Polysemy.Error

import           Language.Metro.Err
import           Language.Metro.Generate.Identifier
import           Language.Metro.Simplify.SimplifyErr
import           Language.Metro.Step
import           Language.Metro.Syntax

type instance StepEffs 'CheckedNativeParams = '[Error Err]

instance Step Decl 'CheckedNativeParams where
    step def@(Def _ (T alts)) = do
        for_ alts \(A (DefAlt (T pats) _) _) ->
            for_ pats \pat -> case node pat of
                VarPat ib@(IdentBind (A (Ident s) _))
                    | identifierize s /= s ->
                        throw $ SimplifyErr $ InvalidNativeArgErr ib
                _ -> pure ()
        autoStep def
    step decl = autoStep decl
