{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Language.Metro.Step
    ( StepEffs
    , Step (..)
    , AutoStep (..)
    ) where

import           Data.Singletons.Prelude.Enum
import           Polysemy

import           Language.Metro.Syntax

type family StepEffs (p :: Pass) :: EffectRow

class Step (n :: Node) (p :: Pass) where
    step :: Members (StepEffs p) r => n (Pred p) -> Sem r (n p)

class AutoStep (n :: Node) (p :: Pass) where
    autoStep :: Members (StepEffs p) r => n (Pred p) -> Sem r (n p)

instance {-# OVERLAPPABLE #-} AutoStep n p => Step n p where
    step = autoStep

type CanAutoStep (n :: Node) (p :: Pass) =
    ( AllStep (Children n p) p
    , ListEq (Children n (Pred p)) (Children n p) )

type family AllStep (ns :: [Node]) (p :: Pass) where
    AllStep '[n] p = Step n p
    AllStep (n : ns) p = (Step n p, AllStep ns p)

type family ListEq (xs :: [k]) (ys :: [k]) where
    ListEq '[x] '[y] = x ~ y
    ListEq (x : xs) (y : ys) = (x ~ y, ListEq xs ys)

instance (Step n p, Ann (Pred p) ~ Ann p) => AutoStep (A n) p where
    autoStep (A n a) = flip A a <$> step n

instance AutoStep (P t) p where
    autoStep (P x) = pure $ P x

instance AutoStep U p where
    autoStep U = pure U

instance (Step n p, Traversable t) => AutoStep (T t n) p where
    autoStep (T t) = T <$> traverse step t

instance CanAutoStep Mod p => AutoStep Mod p where
    autoStep (Mod imps tls) = Mod <$> step imps <*> step tls

instance CanAutoStep Import p => AutoStep Import p where
    autoStep (Import path m) = Import <$> step path <*> step m

instance CanAutoStep TopLevel p => AutoStep TopLevel p where
    autoStep (TLDecl expType decl) = TLDecl <$> step expType <*> step decl

instance AutoStep ExpType p where
    autoStep Exp  = pure Exp
    autoStep Priv = pure Priv

instance CanAutoStep Decl p => AutoStep Decl p where
    autoStep (Def name alts) = Def <$> step name <*> step alts
    autoStep (Let name expr) = Let <$> step name <*> step expr

instance CanAutoStep DefAlt p => AutoStep DefAlt p where
    autoStep (DefAlt pats expr) = DefAlt <$> step pats <*> step expr

instance CanAutoStep Pat p => AutoStep Pat p where
    autoStep (VarPat ident) = VarPat <$> step ident
    autoStep (LitPat lit)   = LitPat <$> step lit
    autoStep WildPat        = pure WildPat

instance CanAutoStep Expr p => AutoStep Expr p where
    autoStep (VarExpr ident)      = VarExpr <$> step ident
    autoStep (LitExpr lit)        = LitExpr <$> step lit
    autoStep (App f x)            = App <$> step f <*> step x
    autoStep (If cond t f)        = If <$> step cond <*> step t <*> step f
    autoStep (Case caseHead alts) = Case <$> step caseHead <*> step alts
    autoStep (LamExpr lam)        = LamExpr <$> step lam
    autoStep (NativeExpr n)       = NativeExpr <$> step n

instance CanAutoStep CaseHead p => AutoStep CaseHead p where
    autoStep (CaseHead x) = CaseHead <$> step x

instance CanAutoStep AliExpr p => AutoStep AliExpr p where
    autoStep (AliExpr pat ident) = AliExpr <$> step pat <*> step ident

instance CanAutoStep CaseAlt p => AutoStep CaseAlt p where
    autoStep (CaseAlt altHead expr) = CaseAlt <$> step altHead <*> step expr

instance CanAutoStep Lam p => AutoStep Lam p where
    autoStep (Lam lamHead expr) = Lam <$> step lamHead <*> step expr

instance CanAutoStep Native p => AutoStep Native p where
    autoStep (Native n) = Native <$> step n

instance CanAutoStep IdentBind p => AutoStep IdentBind p where
    autoStep (IdentBind ident) = IdentBind <$> step ident

instance CanAutoStep IdentRef p => AutoStep IdentRef p where
    autoStep (IdentRef bind ident) = IdentRef <$> step bind <*> step ident

instance CanAutoStep Ident p => AutoStep Ident p where
    autoStep (Ident str)            = pure $ Ident str
    autoStep (GenIdentPart pre num) = GenIdentPart <$> step pre <*> step num
    autoStep (GenIdentFull num)     = GenIdentFull <$> step num

instance AutoStep Lit p where
    autoStep (NumLit n) = pure $ NumLit n
    autoStep (StrLit s) = pure $ StrLit s
