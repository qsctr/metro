{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Dtfpl.Step
    ( StepEffs
    , Step (..)
    ) where

import           Data.Singletons.Prelude.Enum
import           Polysemy

import           Language.Dtfpl.Syntax

type family StepEffs (p :: Pass) :: EffectRow

class Step (n :: Node) (p :: Pass) where
    step :: Members (StepEffs p) r => n (Pred p) -> Sem r (n p)

type AutoStep (n :: Node) (p :: Pass) =
    ( AllStep (Children n p) p
    , ListEq (Children n (Pred p)) (Children n p) )

type family AllStep (ns :: [Node]) (p :: Pass) where
    AllStep '[n] p = Step n p
    AllStep (n : ns) p = (Step n p, AllStep ns p)

type family ListEq (xs :: [k]) (ys :: [k]) where
    ListEq '[x] '[y] = x ~ y
    ListEq (x : xs) (y : ys) = (x ~ y, ListEq xs ys)

instance {-# OVERLAPPABLE #-}
    (Step n p, Ann (Pred p) ~ Ann p) => Step (A n) p where
    step (A n a) = flip A a <$> step n

instance Step (P t) p where
    step (P x) = pure $ P x

instance {-# OVERLAPPABLE #-} (Step n p, Traversable t) => Step (T t n) p where
    step (T t) = T <$> traverse step t

instance AutoStep Prog p => Step Prog p where
    step (Prog imps tls) = Prog <$> step imps <*> step tls

instance AutoStep Import p => Step Import p where
    step (Import m) = Import <$> step m

instance AutoStep TopLevel p => Step TopLevel p where
    step (TLDecl expType decl) = TLDecl <$> step expType <*> step decl

instance Step ModuleName p where
    step (ModuleName atoms) = ModuleName <$> step atoms

instance Step ModuleAtom p where
    step (ModuleAtom str) = pure $ ModuleAtom str

instance Step ExpType p where
    step Exp  = pure Exp
    step Priv = pure Priv

instance {-# OVERLAPPABLE #-} AutoStep Decl p => Step Decl p where
    step (Def name alts) = Def <$> step name <*> step alts
    step (Let name expr) = Let <$> step name <*> step expr

instance AutoStep DefAlt p => Step DefAlt p where
    step (DefAlt pats expr) = DefAlt <$> step pats <*> step expr

instance AutoStep Pat p => Step Pat p where
    step (VarPat ident) = VarPat <$> step ident
    step (LitPat lit)   = LitPat <$> step lit
    step WildPat        = pure WildPat

instance AutoStep Expr p => Step Expr p where
    step (VarExpr ident)      = VarExpr <$> step ident
    step (LitExpr lit)        = LitExpr <$> step lit
    step (App f x)            = App <$> step f <*> step x
    step (If cond true false) = If <$> step cond <*> step true <*> step false
    step (Case caseHead alts) = Case <$> step caseHead <*> step alts
    step (LamExpr lam)        = LamExpr <$> step lam
    step (NativeExpr n)       = NativeExpr <$> step n

instance {-# OVERLAPPABLE #-} AutoStep CaseHead p => Step CaseHead p where
    step (CaseHead x) = CaseHead <$> step x

instance AutoStep AliExpr p => Step AliExpr p where
    step (AliExpr pat ident) = AliExpr <$> step pat <*> step ident

instance {-# OVERLAPPABLE #-} AutoStep CaseAlt p => Step CaseAlt p where
    step (CaseAlt altHead expr) = CaseAlt <$> step altHead <*> step expr

instance {-# OVERLAPPABLE #-} AutoStep Lam p => Step Lam p where
    step (Lam lamHead expr) = Lam <$> step lamHead <*> step expr

instance {-# OVERLAPPABLE #-} AutoStep Native p => Step Native p where
    step (Native n) = Native <$> step n

instance {-# OVERLAPPABLE #-} AutoStep IdentBind p => Step IdentBind p where
    step (IdentBind ident) = IdentBind <$> step ident

instance {-# OVERLAPPABLE #-} AutoStep IdentRef p => Step IdentRef p where
    step (IdentRef ident) = IdentRef <$> step ident

instance {-# OVERLAPPABLE #-} AutoStep Ident p => Step Ident p where
    step (Ident str)               = pure $ Ident str
    step (GenIdentPart prefix num) = GenIdentPart <$> step prefix <*> step num
    step (GenIdentFull num)        = GenIdentFull <$> step num

instance Step Lit p where
    step (NumLit n) = pure $ NumLit n
    step (StrLit s) = pure $ StrLit s
