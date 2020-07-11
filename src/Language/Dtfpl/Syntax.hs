{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

-- | Abstract syntax tree and related types and functions.
--
-- Using type families and data kinds, we can represent all forms of the syntax
-- tree in each stage of the simplification process with a single set of ADTs.
-- Each AST node is parameterized by the current simplification pass.
module Language.Dtfpl.Syntax
    ( Pass (..)
    , Core
    , Node
    , P (..)
    , U (..)
    , T (..)
    , Children
    , A (..)
    , Ann
    , Mod (..)
    , Import (..)
    , TopLevel (..)
    , ModName (..)
    , ModAtom (..)
    , ExpType (..)
    , Decl (..)
    , DefHead
    , DefBody
    , DefAlt (..)
    , Pat (..)
    , Expr (..)
    , CaseHead (..)
    , CaseHead'
    , AliExpr (..)
    , CaseAlt (..)
    , Lam (..)
    , LamHead
    , Native (..)
    , Native'
    , IdentBind (..)
    , ImpIdentBind (..)
    , IdentRef (..)
    , IdentRefBind
    , identBindToRef
    , Ident (..)
    , GenIdentPartPrefix
    , GenIdentPartNum
    , GenIdentFullNum
    , Lit (..)
    ) where

import           Data.Data
import           Data.Kind
import           Data.List.NonEmpty
import           Data.Singletons.Prelude.Enum
import           Data.Singletons.TH
import           Numeric.Natural

import           Language.Dtfpl.Parse.Loc
import qualified Language.ECMAScript.Syntax   as JS

-- | Simplification pass.
-- Only used in promoted form.
$(promote [d|
    data Pass
        = Source
        | ParsedNative
        | InitGen
        | NoDef
        | NoLamMatch
        | Curried
        | Resolved
        | Reordered
        | AliasCase
        deriving (Eq, Ord, Enum, Bounded)
    |])

-- | Most simplified pass.
type Core = (MaxBound :: Pass)

-- | All nodes are indexed by the current pass.
type Node = Pass -> Type

-- | Typeclass which takes a single param of kind 'Type'.
type Class = Type -> Constraint

-- | Lift a regular type to a 'Node' type which ignores the current pass.
-- Phantom type.
newtype P t (p :: Pass) = P { unP :: t } deriving (Eq, Ord, Show, Read, Data)

-- | Lifted version of '()' which ignores the current pass.
-- Specialized version of @'P' '()'@.
data U (p :: Pass) = U deriving (Eq, Ord, Show, Read, Data)

-- | Lift a @* -> *@ type constructor to a 'Node' type parameterized by
-- inner node type @n@ and pass @p@ and holding elements of type @n p@.
newtype T (t :: Type -> Type) (n :: Node) (p :: Pass) = T { unT :: t (n p) }
    deriving (Eq, Ord, Show, Read, Data)

-- | Alternative clause for use with 'When'.
-- Only used in promoted form.
data WhenAlt = forall t. WhenAlt Pass t

-- | Infix type operator synonym for 'WhenAlt'.
type (p :: Pass) ==> t = 'WhenAlt p t

-- | Given the current pass, a default type, and a type-level list of
-- alternatives each consisting of a pass and a corresponding type,
-- assumed to be in ascending order by pass, finds the greatest pass in the list
-- which is less than the current pass and returns its corresponding type.
-- If the current pass is less than all passes in the list, then returns the
-- default type.
type family When (p :: Pass) t (xs :: [WhenAlt]) where
    When _ t '[] = t
    When p t ((p' ==> t') : xs) = If (p < p') t (When p t' xs)

-- | Returns the given type if the current pass is at least 'InitGen',
-- otherwise returns 'P Void'.
type FromInitGen (p :: Pass) t = If (p < 'InitGen) (P Void) t

-- | Returns 'P Void' if the current pass is after some specified pass,
-- otherwise returns the given type.
type VoidAfter (p' :: Pass) (p :: Pass) t =
    When p t '[ p' ==> P Void ]

-- | Gets the children of a node at a given pass.
-- For use with generating constraints.
type family Children (n :: Node) (p :: Pass) :: [Node]

-- | Ensures that all the children of the node at the given pass
-- satisfy the typeclass constraint.
type Forall (c :: Class) (n :: Node) (p :: Pass) =
    ToConstraint c (Children n p) p

type ForallTy (c :: Class) (n :: Node) (p :: Pass) = (Forall c n p, Typeable p)

-- | Converts a type-level list of nodes into a constraint which applies
-- the typeclass constraint to each node.
type family ToConstraint (c :: Class) (ns :: [Node]) (p :: Pass) where
    ToConstraint c '[n] p = c (n p)
    ToConstraint c (n : ns) p = (c (n p), ToConstraint c ns p)

-- | Annotated node.
data A (n :: Node) (p :: Pass) = A { node :: n p, ann :: Ann p }

type ForallA (c :: Class) (n :: Node) (p :: Pass) = (c (n p), c (Ann p))

deriving instance ForallA Eq n p => Eq (A n p)
deriving instance ForallA Ord n p => Ord (A n p)
deriving instance ForallA Show n p => Show (A n p)
deriving instance ForallA Read n p => Read (A n p)
deriving instance (ForallA Data n p, Typeable n, Typeable p) => Data (A n p)

-- | The annotation for nodes at the given pass.
type Ann (p :: Pass) = When p
    Loc
    '[ 'InitGen ==> Maybe Loc ]

-- | Module.
data Mod (p :: Pass) = Mod (T [] (A Import) p) (T [] (A TopLevel) p)

type instance Children Mod p =
    '[ T [] (A Import)
     , T [] (A TopLevel) ]

deriving instance Forall Eq Mod p => Eq (Mod p)
deriving instance Forall Ord Mod p => Ord (Mod p)
deriving instance Forall Show Mod p => Show (Mod p)
deriving instance Forall Read Mod p => Read (Mod p)
deriving instance ForallTy Data Mod p => Data (Mod p)

newtype Import (p :: Pass)
    -- | Import statement.
    = Import (A (P ModName) p)

type instance Children Import p =
    '[ A (P ModName) ]

deriving instance Forall Eq Import p => Eq (Import p)
deriving instance Forall Ord Import p => Ord (Import p)
deriving instance Forall Show Import p => Show (Import p)
deriving instance Forall Read Import p => Read (Import p)
deriving instance ForallTy Data Import p => Data (Import p)

newtype ModName = ModName (NonEmpty ModAtom)
    deriving (Eq, Ord, Show, Read, Data)

newtype ModAtom = ModAtom { unModAtom :: String }
    deriving (Eq, Ord, Show, Read, Data)

-- | Top level statement.
data TopLevel (p :: Pass)
    -- | Top level declaration.
    = TLDecl (ExpType p) (A Decl p)

type instance Children TopLevel p =
    '[ ExpType, A Decl ]

deriving instance Forall Eq TopLevel p => Eq (TopLevel p)
deriving instance Forall Ord TopLevel p => Ord (TopLevel p)
deriving instance Forall Show TopLevel p => Show (TopLevel p)
deriving instance Forall Read TopLevel p => Read (TopLevel p)
deriving instance ForallTy Data TopLevel p => Data (TopLevel p)

data ExpType (p :: Pass)
    -- | Exported.
    = Exp
    -- | Private.
    | Priv
    deriving (Eq, Ord, Show, Read, Data)

-- | Declaration.
data Decl (p :: Pass)
    -- | @def@ function declaration.
    = Def (DefHead p p) (DefBody p p)
    -- | @let@ variable declaration.
    | Let (IdentBind p) (A Expr p)

type instance Children Decl p =
    '[ DefHead p, DefBody p
     , IdentBind, A Expr ]

deriving instance Forall Eq Decl p => Eq (Decl p)
deriving instance Forall Ord Decl p => Ord (Decl p)
deriving instance Forall Show Decl p => Show (Decl p)
deriving instance Forall Read Decl p => Read (Decl p)
deriving instance ForallTy Data Decl p => Data (Decl p)

-- | Head of the 'Def' declaration at the given pass.
type DefHead (p :: Pass) = VoidAfter 'NoDef p IdentBind

-- | Body of the 'Def' declaration at the given pass.
type DefBody (p :: Pass) = VoidAfter 'NoDef p (T NonEmpty (A DefAlt))

-- | 'Def' alternative.
data DefAlt (p :: Pass)
    = DefAlt (T NonEmpty (A Pat) p) (A Expr p)

type instance Children DefAlt p =
    '[ T NonEmpty (A Pat), A Expr ]

deriving instance Forall Eq DefAlt p => Eq (DefAlt p)
deriving instance Forall Ord DefAlt p => Ord (DefAlt p)
deriving instance Forall Show DefAlt p => Show (DefAlt p)
deriving instance Forall Read DefAlt p => Read (DefAlt p)
deriving instance ForallTy Data DefAlt p => Data (DefAlt p)

-- | Pattern.
data Pat (p :: Pass)
    -- | Variable pattern.
    -- e.g. @x -> ...@
    = VarPat (IdentBind p)
    -- | Literal pattern.
    -- e.g. @42 -> ...@
    | LitPat (A Lit p)
    -- | Wildcard pattern.
    -- e.g. @_ -> ...@
    | WildPat

type instance Children Pat p =
    '[ IdentBind
     , A Lit ]

deriving instance Forall Eq Pat p => Eq (Pat p)
deriving instance Forall Ord Pat p => Ord (Pat p)
deriving instance Forall Show Pat p => Show (Pat p)
deriving instance Forall Read Pat p => Read (Pat p)
deriving instance ForallTy Data Pat p => Data (Pat p)

-- | Expression.
data Expr (p :: Pass)
    -- | Variable expression.
    = VarExpr (IdentRef p)
    -- | Literal expression.
    | LitExpr (A Lit p)
    -- | Function application.
    -- Multi-argument applications are curried.
    | App (A Expr p) (A Expr p)
    -- | If-then-else conditional expression.
    | If (A Expr p) (A Expr p) (A Expr p)
    -- | Case expression.
    | Case (CaseHead p) (T NonEmpty (A CaseAlt) p)
    -- | Lambda expression.
    | LamExpr (Lam p)
    -- | Native JS expression.
    | NativeExpr (Native p)

type instance Children Expr p =
    '[ IdentRef
     , A Lit
     , A Expr
     , CaseHead, T NonEmpty (A CaseAlt)
     , Lam
     , Native ]

deriving instance Forall Eq Expr p => Eq (Expr p)
deriving instance Forall Ord Expr p => Ord (Expr p)
deriving instance Forall Show Expr p => Show (Expr p)
deriving instance Forall Read Expr p => Read (Expr p)
deriving instance ForallTy Data Expr p => Data (Expr p)

-- | Head of 'Case' expression.
newtype CaseHead (p :: Pass) = CaseHead (CaseHead' p p)

type instance Children CaseHead p =
    '[ CaseHead' p ]

deriving instance Forall Eq CaseHead p => Eq (CaseHead p)
deriving instance Forall Ord CaseHead p => Ord (CaseHead p)
deriving instance Forall Show CaseHead p => Show (CaseHead p)
deriving instance Forall Read CaseHead p => Read (CaseHead p)
deriving instance ForallTy Data CaseHead p => Data (CaseHead p)

-- | Head of 'Case' expression at the given pass.
type CaseHead' (p :: Pass) = When p
    (T NonEmpty (A Expr))
    '[ 'AliasCase ==> T NonEmpty AliExpr ]

-- | Aliased expression.
-- This is used when an identifier needs to be assigned to a non-variable
-- expression, to cache the result of that expression
-- for use in the generated code.
data AliExpr (p :: Pass)
    = AliExpr (A Expr p) (T Maybe IdentBind p)

type instance Children AliExpr p =
    '[ A Expr, T Maybe IdentBind ]

deriving instance Forall Eq AliExpr p => Eq (AliExpr p)
deriving instance Forall Ord AliExpr p => Ord (AliExpr p)
deriving instance Forall Show AliExpr p => Show (AliExpr p)
deriving instance Forall Read AliExpr p => Read (AliExpr p)
deriving instance ForallTy Data AliExpr p => Data (AliExpr p)

-- | 'Case' alternative.
data CaseAlt (p :: Pass)
    = CaseAlt (T NonEmpty (A Pat) p) (A Expr p)

type instance Children CaseAlt p =
    '[ T NonEmpty (A Pat), A Expr ]

deriving instance Forall Eq CaseAlt p => Eq (CaseAlt p)
deriving instance Forall Ord CaseAlt p => Ord (CaseAlt p)
deriving instance Forall Show CaseAlt p => Show (CaseAlt p)
deriving instance Forall Read CaseAlt p => Read (CaseAlt p)
deriving instance ForallTy Data CaseAlt p => Data (CaseAlt p)

-- | Lambda.
data Lam (p :: Pass)
    = Lam (LamHead p p) (A Expr p)

type instance Children Lam p =
    '[ LamHead p, A Expr ]

deriving instance Forall Eq Lam p => Eq (Lam p)
deriving instance Forall Ord Lam p => Ord (Lam p)
deriving instance Forall Show Lam p => Show (Lam p)
deriving instance Forall Read Lam p => Read (Lam p)
deriving instance ForallTy Data Lam p => Data (Lam p)

-- | Head of the lambda at the given pass.
type LamHead (p :: Pass) = When p
    (T NonEmpty (A Pat))
    '[ 'NoLamMatch ==> T NonEmpty IdentBind
     , 'Curried ==> IdentBind ]

-- | Native JS expression.
newtype Native (p :: Pass) = Native (Native' p p)

type instance Children Native p =
    '[ Native' p ]

deriving instance Forall Eq Native p => Eq (Native p)
deriving instance Forall Ord Native p => Ord (Native p)
deriving instance Forall Show Native p => Show (Native p)
deriving instance Forall Read Native p => Read (Native p)
deriving instance ForallTy Data Native p => Data (Native p)

-- | The representation of a native JS expression at the given pass.
type Native' (p :: Pass) = When p
    (P String)
    '[ 'ParsedNative ==> P JS.Expression ]

newtype IdentBind (p :: Pass) = IdentBind { unIdentBind :: A Ident p }

type instance Children IdentBind p =
    '[ A Ident ]

deriving instance Forall Eq IdentBind p => Eq (IdentBind p)
deriving instance Forall Ord IdentBind p => Ord (IdentBind p)
deriving instance Forall Show IdentBind p => Show (IdentBind p)
deriving instance Forall Read IdentBind p => Read (IdentBind p)
deriving instance ForallTy Data IdentBind p => Data (IdentBind p)

data ImpIdentBind = ImpIdentBind ModName (IdentBind Core)
    deriving (Eq, Ord, Show, Read, Data)

data IdentRef (p :: Pass) = IdentRef (IdentRefBind p p) (A Ident p)

type instance Children IdentRef p =
    '[ IdentRefBind p, A Ident ]

deriving instance Forall Eq IdentRef p => Eq (IdentRef p)
deriving instance Forall Ord IdentRef p => Ord (IdentRef p)
deriving instance Forall Show IdentRef p => Show (IdentRef p)
deriving instance Forall Read IdentRef p => Read (IdentRef p)
deriving instance ForallTy Data IdentRef p => Data (IdentRef p)

type IdentRefBind (p :: Pass) = When p
    U
    '[ 'Resolved ==> T (Either ImpIdentBind) IdentBind ]

class IdentBindToRef identRefBind where
    identBindToRef :: IdentRefBind p ~ identRefBind => IdentBind p -> IdentRef p

instance IdentBindToRef U where
    identBindToRef (IdentBind ident) = IdentRef U ident

instance IdentBindToRef (T (Either ImpIdentBind) IdentBind) where
    identBindToRef ib@(IdentBind ident) = IdentRef (T $ Right ib) ident

-- | Identifier.
data Ident (p :: Pass)
    -- | Named identifier.
    = Ident String
    -- | Partially generated identifier, with a prefix from another identifier.
    | GenIdentPart (GenIdentPartPrefix p p) (GenIdentPartNum p p)
    -- | Fully generated identifier.
    | GenIdentFull (GenIdentFullNum p p)

type instance Children Ident p =
    '[ GenIdentPartPrefix p, GenIdentPartNum p
     , GenIdentFullNum p ]

deriving instance Forall Eq Ident p => Eq (Ident p)
deriving instance Forall Ord Ident p => Ord (Ident p)
deriving instance Forall Show Ident p => Show (Ident p)
deriving instance Forall Read Ident p => Read (Ident p)
deriving instance ForallTy Data Ident p => Data (Ident p)

-- | Prefix of a 'GenIdentPart' at the given pass.
type GenIdentPartPrefix (p :: Pass) = FromInitGen p (A Ident)

-- | Number part of a 'GenIdentPart' at the given pass.
type GenIdentPartNum (p :: Pass) = FromInitGen p (P Natural)

-- | Number of a 'GenIdentFull' at the given pass.
type GenIdentFullNum (p :: Pass) = FromInitGen p (P Natural)

-- | Literal.
data Lit (p :: Pass)
    -- | Number literal.
    -- All numbers are 'Double's.
    = NumLit Double
    -- | String literal.
    | StrLit String
    deriving (Eq, Ord, Show, Read, Data)
