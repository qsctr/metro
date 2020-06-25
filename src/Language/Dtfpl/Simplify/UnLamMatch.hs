{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Desugar pattern-matching in 'Lam's.
module Language.Dtfpl.Simplify.UnLamMatch () where

import           Control.Category
import qualified Data.List.NonEmpty                   as N
import           Data.Maybe
import           Data.Traversable

import           Language.Dtfpl.Simplify.GenIdentFull
import           Language.Dtfpl.Step
import           Language.Dtfpl.Syntax
import           Language.Dtfpl.Syntax.Util

type instance StepEffs 'NoLamMatch = '[EGenIdentFull]

-- | Move any pattern-matching done in 'Lam' heads into a 'Case' expression in
-- the body.
--
-- For example,
--
-- > \x 42 "hello" -> body
--
-- is desugared into
--
-- > \x _0 _1 -> case _0, _1 of
-- >     42, "hello" -> body
--
-- Note that @_0@ and @_1@ are generated identifiers, they are not valid source
-- identifiers.
--
-- 'Lam's which do not do any pattern-matching, i.e. all patterns are 'VarPat's,
-- are unchanged (except that the 'VarPat's are replaced with 'Ident's).
instance Step Lam 'NoLamMatch where
    step (Lam (T pats) expr) = do
        (idents, N.toList >>> catMaybes -> case_) <-
            fmap N.unzip $ for pats $ \case
                A (VarPat ident) _ -> do
                    sIdent <- step ident
                    pure (sIdent, Nothing)
                pat -> do
                    ident <- genLocIdentFull
                    sPat <- step pat
                    pure ( IdentBind ident,
                           Just (genLoc $ VarExpr $ IdentRef U ident, sPat) )
        sExpr <- step expr
        pure $ Lam (T idents) $ case N.nonEmpty case_ of
            Nothing -> sExpr
            Just (N.unzip -> (headExprs, altPats)) ->
                genLoc $ Case (CaseHead $ T headExprs) $
                    T $ pure $ genLoc $ CaseAlt (T altPats) sExpr
