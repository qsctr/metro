module Language.Dtfpl.Syntax.Core
    ( AProg
    , Prog (..)
    , Decl (..)
    , DefAlt (..)
    , Pat (..)
    , Expr (..)
    , Ident (..)
    , Literal (..)
    ) where

import Language.Dtfpl.Syntax.A

type AProg a = A' Prog a

data Prog a
    = Prog [A' Decl a]
    deriving Show

data Decl a
    = Def (A Ident a) [A' DefAlt a]
    | Let (A Ident a) (A' Expr a)
    deriving Show

data DefAlt a
    = DefAlt [A' Pat a] (A' Expr a)
    deriving Show

data Pat a
    = VarPat (A Ident a)
    deriving Show

data Expr a
    = Var (A Ident a)
    | Lit (A Literal a)
    | App (A' Expr a) (A' Expr a)
    | If (A' Expr a) (A' Expr a) (A' Expr a)
    deriving Show

data Ident
    = Ident String
    deriving Show

data Literal
    = NumLit Double
    | StrLit String
    deriving Show
