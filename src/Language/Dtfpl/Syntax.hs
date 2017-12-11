module Language.Dtfpl.Syntax where

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
