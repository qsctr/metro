{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Reorder declarations so that all identifiers are defined before they are
-- referenced.
module Language.Dtfpl.Simplify.Reorder () where

import           Control.Monad
import           Data.Foldable
import           Data.Generics.Uniplate.Data
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as M
import           Data.Set                            (Set)
import qualified Data.Set                            as S
import           Data.Singletons.Prelude.Enum
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Polysemy.State
import           Polysemy.Writer

import           Language.Dtfpl.Err
import           Language.Dtfpl.Simplify.SimplifyErr
import           Language.Dtfpl.Step
import           Language.Dtfpl.Syntax
import           Language.Dtfpl.Syntax.Util

type instance StepEffs 'Reordered = '[Error Err]

type Preordered = Pred 'Reordered
type PIdentBind = IdentBind Preordered

data CheckType
    -- | Check all references in a declaration
    = CheckLoad
    -- | Check only identifiers referenced at load time
    | CheckAll
    deriving (Eq, Ord, Show)

-- | Perform a (reverse) topological sort on the top-level declarations.
instance Step (T [] (A TopLevel)) 'Reordered where
    step (T tls) = do
        let binds = map (fst . splitTLDecl) tls
            tlMap = M.fromList $ zip binds tls
            -- First pass: find identifiers referenced by each declaration
            visitAddRefs :: Members
                '[ State (Map PIdentBind (CheckType, Set PIdentBind))
                 , Error Err ] r
                => CheckType -> Set PIdentBind -> PIdentBind -> Sem r Bool
            visitAddRefs checkType visiting bind = case M.lookup bind tlMap of
                Nothing -> pure False
                Just tl
                    | bind `S.member` visiting -> throw $
                        SimplifyErr $ RecursiveDeclErr $ case tl of
                            A (TLDecl _ decl) _ -> decl
                    | otherwise -> do
                        refMap <- get
                        case M.lookup bind refMap of
                            Nothing -> addRefs tl S.empty
                            Just (ct, old)
                                | ct < checkType -> addRefs tl old
                                | otherwise -> pure ()
                        pure True
              where addRefs tl old = do
                        refs <- case checkType of
                            CheckLoad ->
                                let (checkAllRefs, loadExpr) = run $
                                        runWriterAssocR $ loadEval expr
                                in  (++) <$> visitRefs CheckAll checkAllRefs
                                         <*> visitRefs CheckLoad
                                                (getRefs loadExpr)
                            CheckAll -> visitRefs CheckAll $ getRefs expr
                        let refs' = S.union old $ S.fromList refs
                        modify' $ M.insert bind (checkType, refs')
                      where (_, A expr _) = splitTLDecl tl
                            visiting' = S.insert bind visiting
                            visitRefs ct = filterM $ visitAddRefs ct visiting'
        refMap <- execState M.empty $
            for_ binds $ visitAddRefs CheckLoad S.empty
        -- Second pass: determine topological ordering
        let visitTopoSort :: Members
                '[ Output (A TopLevel Preordered)
                 , State (Set PIdentBind)] r
                => PIdentBind -> Sem r ()
            visitTopoSort bind = do
                visited <- get
                when (bind `S.notMember` visited) do
                    let (_, refs) = refMap M.! bind
                    for_ refs visitTopoSort
                    modify' $ S.insert bind
                    output $ tlMap M.! bind
        (tls', _) <- runOutputList $
            execState S.empty $ for_ binds visitTopoSort
        T <$> traverse step tls'
      where getRefs :: Expr Preordered -> [PIdentBind]
            getRefs expr = [ ib | IdentRef ib _ <- universeBi expr ]
            loadEval :: Member (Writer [PIdentBind]) r
                => Expr Preordered -> Sem r (Expr Preordered)
            loadEval expr = case expr of
                App _ _ -> do
                    tell $ getRefs expr
                    pure dummyExpr
                LamExpr _ -> pure dummyExpr
                _ -> descendM loadEval expr
              where dummyExpr = LitExpr $ genLoc $ NumLit 0
