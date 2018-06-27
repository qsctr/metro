{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Desugar pattern-matching in 'Lam's.
module Language.Dtfpl.Simplify.UnLamMatch () where

import qualified Data.List.NonEmpty              as N
import           Data.Traversable

import           Language.Dtfpl.Simplify.GenUtil
import           Language.Dtfpl.Simplify.Sim
import           Language.Dtfpl.Syntax

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
instance Sim Lam 'NoLamMatch where
    sim (Lam (T pats) expr) = do
        idents <- for pats $ \case
            A (VarPat ident) _ -> sim ident
            _ -> genLocIdentFull
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
