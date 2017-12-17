module Language.Dtfpl.Syntax where

data Prog a
    = Prog a [Decl a]
    deriving Show

data Decl a
    = Def a (Ident a) [DefAlt a]
    | Let a (Ident a) (Expr a)
    deriving Show

data DefAlt a
    = DefAlt a [Pat a] (Expr a)
    deriving Show

data Pat a
    = VarPat a (Ident a)
    deriving Show

data Expr a
    = Var a (Ident a)
    | Lit a (Literal a)
    | App a (Expr a) (Expr a)
    deriving Show

data Ident a
    = Ident a String
    deriving Show

data Literal a
    = NumLit a Double
    | StrLit a String
    deriving Show
