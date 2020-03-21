{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fno-warn-orphans                 #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

-- | Desugar 'Def' declarations.
module Language.Dtfpl.Simplify.UnDef () where

import           Data.Foldable
import qualified Data.List.NonEmpty              as N
import           Data.Traversable

import           Language.Dtfpl.Simplify.GenUtil
import           Language.Dtfpl.Step
import           Language.Dtfpl.Syntax
import           Language.Dtfpl.Syntax.Util

type instance StepClass' 'NoDef m = ()

-- | Replace all 'Def' declarations with 'Let' and 'LamExpr's.
--
-- For example, replaces
--
-- > def func
-- >     x -> y
--
-- with
--
-- > let func = \x -> y
--
-- If there is any pattern-matching done in the 'Def', it will be desugared into
-- a 'Case' expression.
--
-- For example, replaces
--
-- > def func
-- >     1 2 -> a
-- >     3 4 -> b
-- >     x y -> c
--
-- with
--
-- > let func = \x y -> case x, y of
-- >     1, 2 -> a
-- >     3, 4 -> b
-- >     _, _ -> c
--
-- The first 'VarPat' in each column of the 'DefAlt's will be used as the lambda
-- parameter name for that parameter. If there is no 'VarPat' in that column,
-- a 'GenIdentPart' based on the name of the function will be used.
instance Step Decl 'NoDef where
    step (Def name (T alts)) = do
        sName <- step name
        let (patRows, exprs) = N.unzip $
                N.map (\(A (DefAlt (T pats) expr) _) -> (pats, expr)) alts
            patCols = N.transpose patRows
        sExprs <- traverse step exprs
        lamParams <- runGenIdentPart (unIdentBind sName) $ for patCols $ \col ->
            case find nodeIsVarPat col of
                Just varPat -> step varPat
                Nothing     -> genLoc . VarPat . IdentBind <$> genLocIdentPart
        let lamParamIdents = N.map (\(A (VarPat ident) _) -> ident) lamParams
            nonVarPat = N.filter (any (not . nodeIsVarPat) . snd) $
                N.zip lamParamIdents patCols
        lamBody <- case nonVarPat of
            [] -> pure $ N.head sExprs -- TODO: check if more than one expr
            (unzip -> (idents', cols')) -> do
                let caseHead = CaseHead $ T $ N.fromList $
                        map (genLoc . VarExpr . identBindToRef) idents'
                    rows' = N.transpose $ N.fromList cols'
                    rows'' = N.map (N.map (mapNode varPatToWildPat)) rows'
                sRows <- traverse (traverse step) rows''
                let caseAlts = [ A (CaseAlt (T pats) expr) loc
                        | pats <- sRows | expr <- sExprs | A _ loc <- alts ]
                pure $ genLoc $ Case caseHead $ T caseAlts
        pure $ Let sName $ genLoc $ LamExpr $ Lam (T lamParams) lamBody
      where nodeIsVarPat (A (VarPat _) _) = True
            nodeIsVarPat _                = False
            varPatToWildPat (VarPat _) = WildPat
            varPatToWildPat pat        = pat
    step (Let name expr) = Let <$> step name <*> step expr
