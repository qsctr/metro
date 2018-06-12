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
    , P (..)
    , T (..)
    , Children
    , A (..)
    , Ann
    , mapNode
    , Prog (..)
    , Decl (..)
    , DefHead
    , DefBody
    , DefAlt (..)
    , Pat (..)
    , Expr (..)
    , CaseHead (..)
    , CaseHead'
    , CaseAlt (..)
    , CaseAltHead
    , Lam (..)
    , LamHead
    , Ident (..)
    , GenIdentPartPrefix
    , GenIdentPartNum
    , GenIdentFullNum
    , Lit (..)
    ) where

import           Data.Data
import           Data.Kind
import           Data.List.NonEmpty
import           Data.Promotion.Prelude      ((:==$))
import           Data.Promotion.Prelude.Enum
import           Data.Promotion.TH
import           Data.Void
import           GHC.Exts
import           Numeric.Natural

import           Language.Dtfpl.Parser.Loc

$(promote [d|
    data Pass
        = Source
        | InitGen
        | MultiCase
        | NoDef
        | NoLamMatch
        | Curried
        deriving (Eq, Ord, Enum, Bounded)
    |])

type Core = (MaxBound :: Pass)

type Node = Pass -> Type

type Class = Type -> Constraint

data P t (p :: Pass) = P t
    deriving (Eq, Show, Typeable, Data)

data T (t :: * -> *) (n :: Node) (p :: Pass) = T { unT :: t (n p) }
    deriving (Eq, Show, Typeable, Data)

data WhenAlt = forall t. WhenAlt Pass t

type (p :: Pass) ==> t = 'WhenAlt p t

type family When (p :: Pass) t (xs :: [WhenAlt]) where
    When _ t '[] = t
    When p t ((p' ==> t') : xs) = If (p :< p') t (When p t' xs)

type FromInitGen (p :: Pass) t = If (p :< 'InitGen) (P Void) t

type VoidAfter (p' :: Pass) (p :: Pass) t =
    When p t '[ p' ==> P Void ]

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
    '[ 'InitGen ==> Maybe Loc ]

mapNode :: (n p -> n' p) -> A n p -> A n' p
mapNode f (A n a) = A (f n) a

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
    | WildPat
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
    | Case (CaseHead p) (T NonEmpty (A CaseAlt) p)
    | LamExpr (Lam p)
    deriving Typeable

type instance Children Expr p =
    '[ A Ident
     , A Lit
     , A Expr
     , CaseHead, T NonEmpty (A CaseAlt)
     , Lam ]

deriving instance Forall Eq Expr p => Eq (Expr p)
deriving instance Forall Show Expr p => Show (Expr p)
deriving instance (Forall Data Expr p, Typeable p) => Data (Expr p)

data CaseHead (p :: Pass)
    = CaseHead (CaseHead' p p)
    deriving Typeable

type instance Children CaseHead p =
    '[ CaseHead' p ]

deriving instance Forall Eq CaseHead p => Eq (CaseHead p)
deriving instance Forall Show CaseHead p => Show (CaseHead p)
deriving instance (Forall Data CaseHead p, Typeable p) => Data (CaseHead p)

type CaseHead' (p :: Pass) = When p
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

data Lam (p :: Pass)
    = Lam (LamHead p p) (A Expr p)
    deriving Typeable

type instance Children Lam p =
    '[ LamHead p, A Expr ]

deriving instance Forall Eq Lam p => Eq (Lam p)
deriving instance Forall Show Lam p => Show (Lam p)
deriving instance (Forall Data Lam p, Typeable p) => Data (Lam p)

type LamHead (p :: Pass) = When p
    (T NonEmpty (A Pat))
    '[ 'NoLamMatch ==> T NonEmpty (A Ident)
     , 'Curried ==> A Ident ]

data Ident (p :: Pass)
    = Ident String
    | GenIdentPart (GenIdentPartPrefix p p) (GenIdentPartNum p p)
    | GenIdentFull (GenIdentFullNum p p)
    deriving Typeable

type instance Children Ident p =
    '[ GenIdentPartPrefix p, GenIdentPartNum p
     , GenIdentFullNum p ]

deriving instance Forall Eq Ident p => Eq (Ident p)
deriving instance Forall Show Ident p => Show (Ident p)
deriving instance (Forall Data Ident p, Typeable p) => Data (Ident p)

type GenIdentPartPrefix (p :: Pass) = FromInitGen p (A Ident)
type GenIdentPartNum (p :: Pass) = FromInitGen p (P Natural)
type GenIdentFullNum (p :: Pass) = FromInitGen p (P Natural)

data Lit (p :: Pass)
    = NumLit Double
    | StrLit String
    deriving (Eq, Show, Typeable, Data)
