{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Reorder declarations so that all identifiers are defined before they are
-- referenced.
module Language.Metro.Simplify.Reorder () where

import           Control.Monad
import           Data.Foldable
import           Data.Functor
import           Data.Generics.Uniplate.Data
import qualified Data.List.NonEmpty                  as N
import qualified Data.Map.Strict                     as M
import           Data.Set                            (Set)
import qualified Data.Set                            as S
import           Data.Singletons.Prelude.Enum
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Polysemy.State
import           Polysemy.Writer

import           Language.Metro.Err
import           Language.Metro.Simplify.SimplifyErr
import           Language.Metro.Step
import           Language.Metro.Syntax
import           Language.Metro.Syntax.Util

type instance StepEffs 'Reordered = '[Error Err]

type Preordered = Pred 'Reordered
type PIdentBind = IdentBind Preordered

newtype AppRefs = AppRefs (Set PIdentBind) deriving (Semigroup, Monoid)
newtype LamRefs = LamRefs (Set PIdentBind) deriving (Semigroup, Monoid)

data ExprRefs = ExprRefs
    { loadRefs :: Set PIdentBind
    , appRefs  :: Set PIdentBind
    , lamRefs  :: Set PIdentBind }

-- | Perform a (reverse) topological sort on the top-level declarations.
instance Step (T [] (A TopLevel)) 'Reordered where
    step (T tls) = do
        (tls', _) <- runOutputList $
            execState S.empty $ for_ binds $ visit []
        T <$> traverse step tls'
      where binds = getTLBinds tls
            tlMap = M.fromList $ zip binds tls
            visit :: Members
                '[ Output (A TopLevel Preordered) -- topological ordering
                 , State (Set PIdentBind)         -- visited nodes
                 , Error Err ] r
                => [PIdentBind] -> PIdentBind -> Sem r ()
            visit visiting bind = do
                visited <- get
                when (bind `S.notMember` visited)
                    if bind `elem` visiting
                        then throw $ SimplifyErr $ CyclicDeclErr (tlToDecl tl) $
                            N.fromList $ map (tlToDecl . (tlMap M.!)) visiting
                        else do
                            for_ (refMap M.! bind) $ visit (bind : visiting)
                            modify' $ S.insert bind
                            output tl
              where tl = tlMap M.! bind
                    tlToDecl (A (TLDecl _ decl) _) = decl
            exprMap = M.fromList $ map splitTLDecl tls
            exprRefMap = M.map getExprRefs exprMap
            refMap = M.map getAllRefs exprRefMap
            getAllRefs :: ExprRefs -> Set PIdentBind
            getAllRefs ExprRefs {..} = S.union loadRefs $
                run $ execState S.empty $ traverse_ getRunRefs appRefs
            getRunRefs :: Member (State (Set PIdentBind)) r
                => PIdentBind -> Sem r ()
            getRunRefs bind = do
                refs <- get
                when (bind `S.notMember` refs) do
                    modify' $ S.insert bind
                    traverse_ getRunRefs $ lamRefs $ exprRefMap M.! bind
            getExprRefs :: A Expr Preordered -> ExprRefs
            getExprRefs (A expr _) =
                let (AppRefs appRefs, (LamRefs lamRefs, loadExpr)) =
                        run $ runWriter $ runWriter $ go expr
                    loadRefs = S.union appRefs $ getRefs loadExpr
                in  ExprRefs {..}
              where go :: Members '[Writer AppRefs, Writer LamRefs] r
                        => Expr Preordered -> Sem r (Expr Preordered)
                    go e = case e of
                        App _ _   -> tell (AppRefs $ getRefs e) $> dummyExpr
                        LamExpr _ -> tell (LamRefs $ getRefs e) $> dummyExpr
                        _         -> descendM go e
                    getRefs :: Expr Preordered -> Set PIdentBind
                    getRefs e = S.fromList
                        [ ib | IdentRef (T (Right ib)) _ <- universeBi e
                             , M.member ib tlMap ]
                    dummyExpr :: Expr Preordered
                    dummyExpr = LitExpr $ genLoc $ NumLit 0
