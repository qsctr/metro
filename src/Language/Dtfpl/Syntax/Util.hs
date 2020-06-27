{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies   #-}

module Language.Dtfpl.Syntax.Util
    ( mapNode
    , genLoc
    , absurdP
    , mapTLDecl
    , splitTLDecl
    , isSourceIdent
    ) where

import           Data.Coerce
import           Data.Void

import           Language.Dtfpl.Syntax

-- | Applies a function to the unannotated node inside an annotated node.
mapNode :: Ann p ~ Ann p' => (n p -> n' p') -> A n p -> A n' p'
mapNode f (A n a) = A (f n) a

-- | Annotate a node as a generated node.
genLoc :: Ann p ~ Maybe a => n p -> A n p
genLoc = flip A Nothing

-- | Lifted version of 'absurd' for @'P' 'Void'@.
absurdP :: P Void p -> a
absurdP (P x) = absurd x

-- | Map into the 'Decl' part of a 'TLDecl'.
mapTLDecl :: (A Decl p -> A Decl p') -> TopLevel p -> TopLevel p'
mapTLDecl f (TLDecl expType decl) = TLDecl (coerce expType) $ f decl

-- | Get a top-level declaration as a (name, value) pair.
-- Requires all declarations to already have been simplified to 'Let's.
splitTLDecl :: DefHead p ~ P Void => A TopLevel p -> (IdentBind p, A Expr p)
splitTLDecl (A (TLDecl _ (A decl _)) _) = case decl of
    Let bind body -> (bind, body)
    Def bind _    -> absurdP bind

-- | Returns 'True' if the identifier is not compiler-generated.
isSourceIdent :: Ident p -> Bool
isSourceIdent (Ident _)          = True
isSourceIdent (GenIdentPart _ _) = False
isSourceIdent (GenIdentFull _)   = False
