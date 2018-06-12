{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns          #-}

module Language.Dtfpl.Simplify.MultiCase () where

import qualified Data.List.NonEmpty              as N
import           Data.Traversable

import           Language.Dtfpl.Simplify.GenUtil
import           Language.Dtfpl.Simplify.Sim
import           Language.Dtfpl.Syntax

instance Sim Lam 'NoLamMatch where
    sim (Lam (T pats) expr) = do
        idents <- for pats $ \case
            A (VarPat ident) _ -> sim ident
            _ -> genIdentFull
        Lam (T idents) <$>
            case N.filter (isGenIdentFull . node . fst) $ N.zip idents pats of
                [] -> sim expr
                (unzip -> (idents', pats')) ->
                    let caseHead = CaseHead $ T $ N.fromList $
                            map (genLoc . VarExpr) idents'
                    in  fmap (genLoc . Case caseHead . T . pure . genLoc) $
                            CaseAlt <$> sim (T $ N.fromList pats') <*> sim expr
      where isGenIdentFull (GenIdentFull _) = True
            isGenIdentFull _                = False
