{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Language.Dtfpl.Syntax
    ( Pass (..)
    , A (..)
    , Prog (..)
    , Decl (..)
    , DefAlt (..)
    , Pat (..)
    , Expr (..)
    , CaseAlt (..)
    , Ident (..)
    , Lit (..)
    ) where

import           Data.List.NonEmpty
import           Data.Promotion.TH
import           Data.Void
import           GHC.Exts

import           Language.Dtfpl.Parser.Loc

data Pass
    = Source
    | MultiCase
    | NoDef
    | NoLamMatch
    | Curried

$(promoteEqInstance ''Pass)
$(promoteOrdInstance ''Pass)

data WhenAlt t = WhenAlt Pass t

type (p :: Pass) ==> t = 'WhenAlt p t

type family When (p :: Pass) t (xs :: [WhenAlt *]) where
    When _ t '[] = t
    When p t ((p' ==> t') : xs) = If (p :< p') t (When p t' xs)

type VoidAfter (p' :: Pass) (p :: Pass) t =
    When p t '[ p' ==> Void ]

type family C (c :: * -> Constraint) (n :: Pass -> *) (p :: Pass) :: Constraint

data A (n :: Pass -> *) (p :: Pass) = A { node :: n p, ann :: Ann p }

deriving instance (Eq (n p), Eq (Ann p)) => Eq (A n p)
deriving instance (Show (n p), Show (Ann p)) => Show (A n p)

type Ann (p :: Pass) = When p
    Loc
    '[ 'NoDef ==> Maybe Loc ]

data Prog (p :: Pass)
    = Prog [A Decl p]

type instance C c Prog p =
    ( c (A Decl p) )

deriving instance C Eq Prog p => Eq (Prog p)
deriving instance C Show Prog p => Show (Prog p)

data Decl (p :: Pass)
    = Def (DefHead p) (DefBody p)
    | Let (A Ident p) (A Expr p)

type instance C c Decl p =
    ( c (DefHead p), c (DefBody p)
    , c (A Ident p), c (A Expr p) )

deriving instance C Eq Decl p => Eq (Decl p)
deriving instance C Show Decl p => Show (Decl p)

type DefHead (p :: Pass) = VoidAfter 'NoDef p (A Ident p)

type DefBody (p :: Pass) = VoidAfter 'NoDef p (NonEmpty (A DefAlt p))

data DefAlt (p :: Pass)
    = DefAlt (NonEmpty (A Pat p)) (A Expr p)

type instance C c DefAlt p =
    ( c (A Pat p), c (A Expr p) )

deriving instance C Eq DefAlt p => Eq (DefAlt p)
deriving instance C Show DefAlt p => Show (DefAlt p)

data Pat (p :: Pass)
    = VarPat (A Ident p)
    | LitPat (A Lit p)

type instance C c Pat p =
    ( c (A Ident p)
    , c (A Lit p) )

deriving instance C Eq Pat p => Eq (Pat p)
deriving instance C Show Pat p => Show (Pat p)

data Expr (p :: Pass)
    = VarExpr (A Ident p)
    | LitExpr (A Lit p)
    | App (A Expr p) (A Expr p)
    | If (A Expr p) (A Expr p) (A Expr p)
    | Case (CaseHead p) (NonEmpty (A CaseAlt p))
    | Lam (LamHead p) (A Expr p)

type instance C c Expr p =
    ( c (A Ident p)
    , c (A Lit p)
    , c (A Expr p)
    , c (CaseHead p), c (A CaseAlt p)
    , c (LamHead p) )

deriving instance C Eq Expr p => Eq (Expr p)
deriving instance C Show Expr p => Show (Expr p)

type CaseHead (p :: Pass) = When p
    (A Expr p)
    '[ 'MultiCase ==> NonEmpty (A Expr p) ]

data CaseAlt (p :: Pass)
    = CaseAlt (CaseAltHead p) (A Expr p)

type instance C c CaseAlt p =
    ( c (CaseAltHead p), c (A Expr p) )

deriving instance C Eq CaseAlt p => Eq (CaseAlt p)
deriving instance C Show CaseAlt p => Show (CaseAlt p)

type CaseAltHead (p :: Pass) = When p
    (A Pat p)
    '[ 'MultiCase ==> NonEmpty (A Pat p) ]

type LamHead (p :: Pass) = When p
    (NonEmpty (A Pat p))
    '[ 'NoLamMatch ==> NonEmpty (A Ident p)
     , 'Curried ==> A Ident p ]

data Ident (p :: Pass)
    = Ident String
    deriving (Eq, Show)

data Lit (p :: Pass)
    = NumLit Double
    | StrLit String
    deriving (Eq, Show)
