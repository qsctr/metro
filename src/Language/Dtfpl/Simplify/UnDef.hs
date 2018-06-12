{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ParallelListComp      #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fno-warn-orphans                 #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Language.Dtfpl.Simplify.UnDef () where

import           Data.Foldable
import qualified Data.List.NonEmpty              as N
import           Data.Traversable

import           Language.Dtfpl.Simplify.GenUtil
import           Language.Dtfpl.Simplify.Sim
import           Language.Dtfpl.Syntax

instance Sim Decl 'NoDef where
    sim (Def name (T alts)) = do
        simName <- sim name
        let (patRows, exprs) = N.unzip $
                N.map (\(A (DefAlt (T pats) expr) _) -> (pats, expr)) alts
            patCols = N.transpose patRows
        simExprs <- traverse sim exprs
        lamParams <- runGenIdentPart simName $ for patCols $ \col ->
            case find nodeIsVarPat col of
                Just varPat -> liftSim varPat
                Nothing     -> genLoc . VarPat <$> genIdentPart
        let lamParamIdents = N.map (\(A (VarPat ident) _) -> ident) lamParams
            nonVarPat = N.filter (any (not . nodeIsVarPat) . snd) $
                N.zip lamParamIdents patCols
        lamBody <- case nonVarPat of
            [] -> pure $ N.head simExprs -- TODO: check if more than one expr
            (unzip -> (idents', cols')) -> do
                let caseHead = CaseHead $ T $ N.fromList $
                        map (genLoc . VarExpr) idents'
                    rows' = N.transpose $ N.fromList cols'
                    rows'' = N.map (N.map (mapNode varPatToWildPat)) rows'
                simRows <- traverse (traverse sim) rows''
                let caseAlts = [ A (CaseAlt (T pats) expr) loc
                        | pats <- simRows | expr <- simExprs | A _ loc <- alts ]
                pure $ genLoc $ Case caseHead $ T caseAlts
        pure $ Let simName $ genLoc $ LamExpr $ Lam (T lamParams) lamBody
      where nodeIsVarPat (A (VarPat _) _) = True
            nodeIsVarPat _                = False
            varPatToWildPat (VarPat _) = WildPat
            varPatToWildPat pat        = pat
    sim (Let name expr) = Let <$> sim name <*> sim expr
