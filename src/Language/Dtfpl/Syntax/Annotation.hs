module Language.Dtfpl.Syntax.Annotation
    ( A (..)
    , A'
    ) where

data A n a = A { node :: n, ann :: a } deriving Show

type A' n a = A (n a) a
