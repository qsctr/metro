{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.Dtfpl.Syntax
    ( Pass (..)
    , Core
    , Node
    , T (..)
    , Children
    , A (..)
    , Ann
    , Prog (..)
    , Decl (..)
    , DefAlt (..)
    , Pat (..)
    , Expr (..)
    , CaseAlt (..)
    , Ident (..)
    , Lit (..)
    ) where

import           Data.Data
import           Data.Kind
import           Data.List.NonEmpty
import           Data.Promotion.Prelude      ((:==$))
import           Data.Promotion.Prelude.Enum
import           Data.Promotion.TH
import           GHC.Exts

import           Language.Dtfpl.Parser.Loc

$(promote [d|
    data Pass
        = Source
        | MultiCase
        | NoDef
        | NoLamMatch
        | Curried
        deriving (Eq, Ord, Enum, Bounded)
    |])

type Core = MaxBound Pass

type Node = Pass -> Type

type Class = Type -> Constraint

data VoidP (p :: Pass)

data T (t :: * -> *) (n :: Node) (p :: Pass) = T (t (n p))
    deriving (Eq, Show, Typeable, Data)

data WhenAlt = forall t. WhenAlt Pass t

type (p :: Pass) ==> t = 'WhenAlt p t

type family When (p :: Pass) t (xs :: [WhenAlt]) where
    When _ t '[] = t
    When p t ((p' ==> t') : xs) = If (p :< p') t (When p t' xs)

type VoidAfter (p' :: Pass) (p :: Pass) t =
    When p t '[ p' ==> VoidP ]

type family Children (n :: Node) (p :: Pass) :: [Node]

type Forall (c :: Class) (n :: Node) (p :: Pass) =
    ToConstraint c (Children n p) p

type family ToConstraint (c :: Class) (ns :: [Node]) (p :: Pass) where
    ToConstraint c '[n] p = c (n p)
    ToConstraint c (n : ns) p = (c (n p), ToConstraint c ns p)

data A (n :: Node) (p :: Pass) = A { node :: n p, ann :: Ann p }
    deriving Typeable

deriving instance (Eq (n p), Eq (Ann p)) => Eq (A n p)
deriving instance (Show (n p), Show (Ann p)) => Show (A n p)
deriving instance (Typeable n, Typeable p, Data (n p), Data (Ann p))
    => Data (A n p)

type Ann (p :: Pass) = When p
    Loc
    '[ 'NoDef ==> Maybe Loc ]

data Prog (p :: Pass)
    = Prog (T [] (A Decl) p)
    deriving Typeable

type instance Children Prog p =
    '[ T [] (A Decl) ]

deriving instance Forall Eq Prog p => Eq (Prog p)
deriving instance Forall Show Prog p => Show (Prog p)
deriving instance (Forall Data Prog p, Typeable p) => Data (Prog p)

data Decl (p :: Pass)
    = Def (DefHead p p) (DefBody p p)
    | Let (A Ident p) (A Expr p)
    deriving Typeable

type instance Children Decl p =
    '[ DefHead p, DefBody p
     , A Ident, A Expr ]

deriving instance Forall Eq Decl p => Eq (Decl p)
deriving instance Forall Show Decl p => Show (Decl p)
deriving instance (Forall Data Decl p, Typeable p) => Data (Decl p)

type DefHead (p :: Pass) = VoidAfter 'NoDef p (A Ident)

type DefBody (p :: Pass) = VoidAfter 'NoDef p (T NonEmpty (A DefAlt))

data DefAlt (p :: Pass)
    = DefAlt (T NonEmpty (A Pat) p) (A Expr p)
    deriving Typeable

type instance Children DefAlt p =
    '[ T NonEmpty (A Pat), A Expr ]

deriving instance Forall Eq DefAlt p => Eq (DefAlt p)
deriving instance Forall Show DefAlt p => Show (DefAlt p)
deriving instance (Forall Data DefAlt p, Typeable p) => Data (DefAlt p)

data Pat (p :: Pass)
    = VarPat (A Ident p)
    | LitPat (A Lit p)
    deriving Typeable

type instance Children Pat p =
    '[ A Ident
     , A Lit ]

deriving instance Forall Eq Pat p => Eq (Pat p)
deriving instance Forall Show Pat p => Show (Pat p)
deriving instance (Forall Data Pat p, Typeable p) => Data (Pat p)

data Expr (p :: Pass)
    = VarExpr (A Ident p)
    | LitExpr (A Lit p)
    | App (A Expr p) (A Expr p)
    | If (A Expr p) (A Expr p) (A Expr p)
    | Case (CaseHead p p) (T NonEmpty (A CaseAlt) p)
    | Lam (LamHead p p) (A Expr p)
    deriving Typeable

type instance Children Expr p =
    '[ A Ident
     , A Lit
     , A Expr
     , CaseHead p, T NonEmpty (A CaseAlt)
     , LamHead p ]

deriving instance Forall Eq Expr p => Eq (Expr p)
deriving instance Forall Show Expr p => Show (Expr p)
deriving instance (Forall Data Expr p, Typeable p) => Data (Expr p)

type CaseHead (p :: Pass) = When p
    (A Expr)
    '[ 'MultiCase ==> T NonEmpty (A Expr) ]

data CaseAlt (p :: Pass)
    = CaseAlt (CaseAltHead p p) (A Expr p)
    deriving Typeable

type instance Children CaseAlt p =
    '[ CaseAltHead p, A Expr ]

deriving instance Forall Eq CaseAlt p => Eq (CaseAlt p)
deriving instance Forall Show CaseAlt p => Show (CaseAlt p)
deriving instance (Forall Data CaseAlt p, Typeable p) => Data (CaseAlt p)

type CaseAltHead (p :: Pass) = When p
    (A Pat)
    '[ 'MultiCase ==> T NonEmpty (A Pat) ]

type LamHead (p :: Pass) = When p
    (T NonEmpty (A Pat))
    '[ 'NoLamMatch ==> T NonEmpty (A Ident)
     , 'Curried ==> A Ident ]

data Ident (p :: Pass)
    = Ident String
    deriving (Eq, Show, Typeable, Data)

data Lit (p :: Pass)
    = NumLit Double
    | StrLit String
    deriving (Eq, Show, Typeable, Data)
