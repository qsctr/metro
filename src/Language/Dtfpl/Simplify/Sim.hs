{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | 'Sim' typeclass and related types and functions.
--
-- Overlappable instances are required in this module to specify generic
-- simplification code which applies to all passes where the children of the
-- node do not change, which can be overridden by specific instances for
-- specific passes.
module Language.Dtfpl.Simplify.Sim
    ( SimState (..)
    , SimM
    , runSim
    , Sim (..)
    ) where

import           Control.Monad.State
import           Data.Promotion.Prelude.Enum
import           Numeric.Natural

import           Language.Dtfpl.Syntax

-- | State used in the simplification process.
data SimState = SimState {
    -- | The next number for 'GenIdentFull'.
    -- Fully generated identifiers are numbered in order of node traversal
    -- for the whole program.
    nextGenIdentFullNum :: Natural
} deriving (Eq, Show)

-- | Initial 'SimState' value.
initSimState :: SimState
initSimState = SimState
    { nextGenIdentFullNum = 0 }

-- | Simplification monad.
-- Uses 'State' monad to keep track of simplification state.
type SimM = State SimState

-- | Run a simplification with the initial simplification state.
-- This should only be used on a complete program, not on parts of it,
-- as combining those parts would result in a possibly invalid program.
runSim :: SimM a -> a
runSim = flip evalState initSimState

-- | A node that can be simplified by a given pass.
class Sim (n :: Node) (p :: Pass) where
    -- | Single simplification pass.
    -- Simplifies from the previous pass to the current pass.
    sim :: n (Pred p) -> SimM (n p)

-- | Constraint which ensures that all the children of some node at the previous
-- pass and at the current pass remain nominally equal, and that the children at
-- the current pass have 'Sim' instances, so that the node can be generically
-- simplified for all passes that satisfy this constraint.
type AutoSim (n :: Node) (p :: Pass) =
    ( SimChildren (Children n p) p
    , SameChildren (Children n (Pred p)) (Children n p) )

-- | Ensures @Sim n p@ for all nodes @n@ in the given list for the given pass
-- @p@.
type family SimChildren (ns :: [Node]) (p :: Pass) where
    SimChildren '[n] p = Sim n p
    SimChildren (n : ns) p = (Sim n p, SimChildren ns p)

-- | Ensures that each node in the first list is nominally equal to the
-- corresponding node in the second list.
-- Basically @zipWith (~)@ at the type level.
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
    sim WildPat        = pure WildPat

instance AutoSim Expr p => Sim Expr p where
    sim (VarExpr ident)      = VarExpr <$> sim ident
    sim (LitExpr lit)        = LitExpr <$> sim lit
    sim (App f x)            = App <$> sim f <*> sim x
    sim (If cond true false) = If <$> sim cond <*> sim true <*> sim false
    sim (Case caseHead alts) = Case <$> sim caseHead <*> sim alts
    sim (LamExpr lam)        = LamExpr <$> sim lam

instance {-# OVERLAPPABLE #-} AutoSim CaseHead p => Sim CaseHead p where
    sim (CaseHead x) = CaseHead <$> sim x

instance AutoSim AliExpr p => Sim AliExpr p where
    sim (AliExpr pat ident) = AliExpr <$> sim pat <*> sim ident

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
