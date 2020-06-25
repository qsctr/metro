{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
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
    , Prog (..)
    , Import (..)
    , TopLevel (..)
    , ModuleName (..)
    , ModuleAtom (..)
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
    , IdentRef (..)
    , IdentBindToRef (..)
    , Ident (..)
    , GenIdentPartPrefix
    , GenIdentPartNum
    , GenIdentFullNum
    , Lit (..)
    ) where

import           Data.Kind
import           Data.List.NonEmpty
import           Data.Singletons.Prelude.Enum
import           Data.Singletons.TH
import           Numeric.Natural

import           Language.Dtfpl.Parser.Loc
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
newtype P t (p :: Pass) = P t deriving (Eq, Show)

-- | Lifted version of '()' which ignores the current pass.
-- Specialized version of @'P' '()'@.
data U (p :: Pass) = U deriving (Eq, Show)

-- | Lift a @* -> *@ type constructor to a 'Node' type parameterized by
-- inner node type @n@ and pass @p@ and holding elements of type @n p@.
newtype T (t :: Type -> Type) (n :: Node) (p :: Pass) = T { unT :: t (n p) }
    deriving (Eq, Show)

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

-- | Converts a type-level list of nodes into a constraint which applies
-- the typeclass constraint to each node.
type family ToConstraint (c :: Class) (ns :: [Node]) (p :: Pass) where
    ToConstraint c '[n] p = c (n p)
    ToConstraint c (n : ns) p = (c (n p), ToConstraint c ns p)

-- | Annotated node.
data A (n :: Node) (p :: Pass) = A { node :: n p, ann :: Ann p }

deriving instance (Eq (n p), Eq (Ann p)) => Eq (A n p)
deriving instance (Show (n p), Show (Ann p)) => Show (A n p)

-- | The annotation for nodes at the given pass.
type Ann (p :: Pass) = When p
    Loc
    '[ 'InitGen ==> Maybe Loc ]

-- | Program.
data Prog (p :: Pass) = Prog (T [] (A Import) p) (T [] (A TopLevel) p)

type instance Children Prog p =
    '[ T [] (A Import)
     , T [] (A TopLevel) ]

deriving instance Forall Eq Prog p => Eq (Prog p)
deriving instance Forall Show Prog p => Show (Prog p)

newtype Import (p :: Pass)
    -- | Import statement.
    = Import (A ModuleName p)

type instance Children Import p =
    '[ A ModuleName ]

deriving instance Forall Eq Import p => Eq (Import p)
deriving instance Forall Show Import p => Show (Import p)

newtype ModuleName (p :: Pass) = ModuleName (T NonEmpty ModuleAtom p)
    deriving (Eq, Show)

newtype ModuleAtom (p :: Pass) = ModuleAtom String deriving (Eq, Show)

-- | Top level statement.
data TopLevel (p :: Pass)
    -- | Top level declaration.
    = TLDecl (ExpType p) (A Decl p)

type instance Children TopLevel p =
    '[ ExpType, A Decl ]

deriving instance Forall Eq TopLevel p => Eq (TopLevel p)
deriving instance Forall Show TopLevel p => Show (TopLevel p)

data ExpType (p :: Pass)
    -- | Exported.
    = Exp
    -- | Private.
    | Priv
    deriving (Eq, Show)

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
deriving instance Forall Show Decl p => Show (Decl p)

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
deriving instance Forall Show DefAlt p => Show (DefAlt p)

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
deriving instance Forall Show Pat p => Show (Pat p)

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
deriving instance Forall Show Expr p => Show (Expr p)

-- | Head of 'Case' expression.
newtype CaseHead (p :: Pass) = CaseHead (CaseHead' p p)

type instance Children CaseHead p =
    '[ CaseHead' p ]

deriving instance Forall Eq CaseHead p => Eq (CaseHead p)
deriving instance Forall Show CaseHead p => Show (CaseHead p)

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
deriving instance Forall Show AliExpr p => Show (AliExpr p)

-- | 'Case' alternative.
data CaseAlt (p :: Pass)
    = CaseAlt (T NonEmpty (A Pat) p) (A Expr p)

type instance Children CaseAlt p =
    '[ T NonEmpty (A Pat), A Expr ]

deriving instance Forall Eq CaseAlt p => Eq (CaseAlt p)
deriving instance Forall Show CaseAlt p => Show (CaseAlt p)

-- | Lambda.
data Lam (p :: Pass)
    = Lam (LamHead p p) (A Expr p)

type instance Children Lam p =
    '[ LamHead p, A Expr ]

deriving instance Forall Eq Lam p => Eq (Lam p)
deriving instance Forall Show Lam p => Show (Lam p)

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
deriving instance Forall Show Native p => Show (Native p)

-- | The representation of a native JS expression at the given pass.
type Native' (p :: Pass) = When p
    (P String)
    '[ 'ParsedNative ==> P JS.Expression ]

newtype IdentBind (p :: Pass) = IdentBind { unIdentBind :: A Ident p }

type instance Children IdentBind p =
    '[ A Ident ]

deriving instance Forall Eq IdentBind p => Eq (IdentBind p)
deriving instance Forall Show IdentBind p => Show (IdentBind p)

data IdentRef (p :: Pass) = IdentRef (IdentRefBind p p) (A Ident p)

type instance Children IdentRef p =
    '[ IdentRefBind p
     , A Ident ]

deriving instance Forall Eq IdentRef p => Eq (IdentRef p)
deriving instance Forall Show IdentRef p => Show (IdentRef p)

type IdentRefBind (p :: Pass) = If (p < 'Resolved) U IdentBind

class IdentBindToRef identRefBind where
    identBindToRef :: IdentRefBind p ~ identRefBind => IdentBind p -> IdentRef p

instance IdentBindToRef U where
    identBindToRef (IdentBind ident) = IdentRef U ident

instance IdentBindToRef IdentBind where
    identBindToRef ib@(IdentBind ident) = IdentRef ib ident

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
deriving instance Forall Show Ident p => Show (Ident p)

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
    deriving (Eq, Show)
