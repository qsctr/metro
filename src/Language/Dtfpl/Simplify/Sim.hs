{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Dtfpl.Simplify.Sim
    ( SimState (..)
    , SimM
    , Sim (..)
    ) where

import           Control.Monad.State
import           Data.Promotion.Prelude.Enum
import           Numeric.Natural

import           Language.Dtfpl.Syntax

data SimState = SimState
    { nextGenIdentFullNum :: Natural }
    deriving (Eq, Show)

type SimM = State SimState

class Sim (n :: Node) (p :: Pass) where
    sim :: n (Pred p) -> SimM (n p)

type AutoSim (n :: Node) (p :: Pass) =
    ( SimChildren (Children n p) p
    , SameChildren (Children n (Pred p)) (Children n p) )

type family SimChildren (ns :: [Node]) (p :: Pass) where
    SimChildren '[n] p = Sim n p
    SimChildren (n : ns) p = (Sim n p, SimChildren ns p)

type family SameChildren (ns :: [Node]) (sns :: [Node]) where
    SameChildren '[n] '[sn] = n ~ sn
    SameChildren (n : ns) (sn : sns) = (n ~ sn, SameChildren ns sns)

instance {-# OVERLAPPABLE #-}
    (Sim n p, Ann (Pred p) ~ Ann p) => Sim (A n) p where
    sim (A n a) = flip A a <$> sim n

instance Sim (P t) p where
    sim (P x) = pure $ P x

instance (Sim n p, Traversable t) => Sim (T t n) p where
    sim (T t) = T <$> traverse sim t

instance AutoSim Prog p => Sim Prog p where
    sim (Prog decls) = Prog <$> sim decls

instance {-# OVERLAPPABLE #-} AutoSim Decl p => Sim Decl p where
    sim (Def name alts) = Def <$> sim name <*> sim alts
    sim (Let name expr) = Let <$> sim name <*> sim expr

instance AutoSim DefAlt p => Sim DefAlt p where
    sim (DefAlt pats expr) = DefAlt <$> sim pats <*> sim expr

instance AutoSim Pat p => Sim Pat p where
    sim (VarPat ident) = VarPat <$> sim ident
    sim (LitPat lit)   = LitPat <$> sim lit

instance AutoSim Expr p => Sim Expr p where
    sim (VarExpr ident)      = VarExpr <$> sim ident
    sim (LitExpr lit)        = LitExpr <$> sim lit
    sim (App f x)            = App <$> sim f <*> sim x
    sim (If cond true false) = If <$> sim cond <*> sim true <*> sim false
    sim (Case caseHead alts) = Case <$> sim caseHead <*> sim alts
    sim (LamExpr lam)        = LamExpr <$> sim lam

instance {-# OVERLAPPABLE #-} AutoSim CaseHead p => Sim CaseHead p where
    sim (CaseHead x) = CaseHead <$> sim x

instance {-# OVERLAPPABLE #-} AutoSim CaseAlt p => Sim CaseAlt p where
    sim (CaseAlt altHead expr) = CaseAlt <$> sim altHead <*> sim expr

instance {-# OVERLAPPABLE #-} AutoSim Lam p => Sim Lam p where
    sim (Lam lamHead expr) = Lam <$> sim lamHead <*> sim expr

instance {-# OVERLAPPABLE #-} AutoSim Ident p => Sim Ident p where
    sim (Ident str)               = pure $ Ident str
    sim (GenIdentPart prefix num) = GenIdentPart <$> sim prefix <*> sim num
    sim (GenIdentFull num)        = GenIdentFull <$> sim num

instance Sim Lit p where
    sim (NumLit n) = pure $ NumLit n
    sim (StrLit s) = pure $ StrLit s
