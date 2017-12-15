module Language.Dtfpl.Syntax where

data Decl a
    = Def a [Alt a]

data Alt a
    = Alt a (Pat a) (Expr a)

data Pat a
    = VarPat a (Ident a)

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
