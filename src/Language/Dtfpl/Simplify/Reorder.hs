{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Reorder declarations so that all identifiers are defined before they are
-- referenced.
module Language.Dtfpl.Simplify.Reorder () where

import           Control.Monad
import           Data.Foldable
import           Data.Generics.Uniplate.Data
import qualified Data.Map.Strict                     as M
import           Data.Set                            (Set)
import qualified Data.Set                            as S
import           Data.Singletons.Prelude.Enum
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Polysemy.State

import           Language.Dtfpl.Err
import           Language.Dtfpl.Simplify.SimplifyErr
import           Language.Dtfpl.Step
import           Language.Dtfpl.Syntax
import           Language.Dtfpl.Syntax.Util

type instance StepEffs 'Reordered = '[Error Err]

type Preordered = Pred 'Reordered
type PIdentBind = IdentBind Preordered

-- | Perform a topological sort on the top-level declarations.
instance Step (T [] (A TopLevel)) 'Reordered where
    step (T tls) = do
        (tls', _) <- runOutputList $
            execState S.empty $ for_ binds $ visit S.empty
        T <$> traverse step tls'
      where binds = map (fst . splitTLDecl) tls
            tlMap = M.fromList $ zip binds tls
            visit :: Members
                '[ Output (A TopLevel Preordered) -- topological ordering
                 , State (Set PIdentBind)         -- visited nodes
                 , Error Err ] r
                => Set PIdentBind -> PIdentBind -> Sem r ()
            visit visiting bind = case M.lookup bind tlMap of
                Nothing -> pure ()
                Just tl -> do
                    visited <- get
                    when (bind `S.notMember` visited)
                        if bind `S.member` visiting
                            then throw $ SimplifyErr $
                                RecursiveLetErr $ case tl of
                                    A (TLDecl _ decl) _ -> decl
                            else do
                                let visiting' = S.insert bind visiting
                                for_ (getLoadRefs tl) \(IdentRef ib _) ->
                                    visit visiting' ib
                                modify' $ S.insert bind
                                output tl
            getLoadRefs :: A TopLevel Preordered -> [IdentRef Preordered]
            getLoadRefs (splitTLDecl -> (_, A expr _)) =
                universeBi $ loadEval expr
            loadEval :: Expr Preordered -> Expr Preordered
            loadEval expr = case expr of
                App _ _   -> expr
                LamExpr _ -> LitExpr $ genLoc $ NumLit 0
                _         -> descend loadEval expr
