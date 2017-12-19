{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeOperators      #-}

module Language.Dtfpl.Syntax where

import           GHC.Generics

class Ann' n where
    ann' :: n a -> a

instance (Ann' n) => Ann' (M1 i c n) where
    ann' (M1 n) = ann' n

instance (Ann' l, Ann' r) => Ann' (l :+: r) where
    ann' (L1 x) = ann' x
    ann' (R1 x) = ann' x

instance (Ann' l, Ann' r) => Ann' (l :*: r) where
    ann' (_ :*: r) = ann' r

instance Ann' Par1 where
    ann' (Par1 a) = a

instance Ann' (K1 i c) where
    ann' _ = undefined

instance Ann' (Rec1 f) where
    ann' _ = undefined

instance Ann' (f :.: g) where
    ann' _ = undefined

class Ann n where
    ann :: n a -> a
    default ann :: (Generic1 n, Ann' (Rep1 n)) => n a -> a
    ann = ann' . from1

data Prog a
    = Prog [Decl a] a
    deriving (Show, Generic1)

instance Ann Prog

data Decl a
    = Def (Ident a) [DefAlt a] a
    | Let (Ident a) (Expr a) a
    deriving (Show, Generic1)

instance Ann Decl

data DefAlt a
    = DefAlt [Pat a] (Expr a) a
    deriving (Show, Generic1)

instance Ann DefAlt

data Pat a
    = VarPat (Ident a) a
    deriving (Show, Generic1)

instance Ann Pat

data Expr a
    = Var (Ident a) a
    | Lit (Literal a) a
    | App (Expr a) (Expr a) a
    deriving (Show, Generic1)

instance Ann Expr

data Ident a
    = Ident String a
    deriving (Show, Generic1)

instance Ann Ident

data Literal a
    = NumLit Double a
    | StrLit String a
    deriving (Show, Generic1)

instance Ann Literal
