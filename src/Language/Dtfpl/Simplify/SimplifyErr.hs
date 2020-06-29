{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE ViewPatterns #-}

-- | Errors for simplify phase
module Language.Dtfpl.Simplify.SimplifyErr
    ( SimplifyErr (..)
    , InternalSimplifyErr (..)
    ) where

import           Data.List.NonEmpty            (NonEmpty)
import qualified Data.List.NonEmpty            as N
import           Data.Singletons.Prelude.Enum

import           Language.Dtfpl.Err.ErrLoc
import           Language.Dtfpl.Err.ErrMessage
import           Language.Dtfpl.Format.Util
import           Language.Dtfpl.Parser.Loc
import           Language.Dtfpl.Syntax
import           Language.Dtfpl.Syntax.Util

-- | Error during simplify phase
data SimplifyErr
    -- | An identifier being defined with the same name as an already existing
    -- one.
    = DuplicateIdentErr
        (A Ident 'Resolved) -- ^ The duplicate identifier
        (IdentBind 'Resolved) -- ^ The original identifier
    -- | Unresolved identifier.
    | UnresolvedIdentErr (A Ident 'Resolved)
    -- | Cyclic declarations.
    | CyclicDeclErr
        (A Decl (Pred 'Reordered)) -- ^ The declaration causing the cycle
        (NonEmpty (A Decl (Pred 'Reordered))) -- ^ "Call stack" of declarations

instance ErrMessage SimplifyErr where
    errMessage (DuplicateIdentErr new old) =
        [ "Duplicate identifier " ++ formatQuote new
        , case ann $ unIdentBind old of
            Nothing -> formatQuote old ++ " is already defined"
            Just loc -> "Previously defined " ++ formatQuote old
                ++ " at " ++ formatLoc loc ]
    errMessage (UnresolvedIdentErr ident) =
        [ "Unresolved identifier " ++ formatQuote ident ]
    errMessage (CyclicDeclErr d ds) =
        "Cyclic declaration" : reverse (lastMsg : msg ds)
      where lastMsg = depends d ++ " again"
            msg (N.uncons -> (x, rest)) = case rest of
                Nothing -> ["The evaluation of " ++ nameLoc x]
                Just xs -> (depends x ++ " which") : msg xs
            depends x = "  depends on " ++ nameLoc x
            nameLoc :: A Decl (Pred 'Reordered) -> String
            nameLoc (A decl a) = case decl of
                Let bind _ -> formatQuote bind ++ case a of
                    Just loc -> " (" ++ formatLoc loc ++ ")"
                    Nothing  -> ""
                Def bind _ -> absurdP bind

instance ErrLoc SimplifyErr where
    errLoc (DuplicateIdentErr new _)  = ann new
    errLoc (UnresolvedIdentErr ident) = ann ident
    errLoc (CyclicDeclErr decl _)     = ann decl

data InternalSimplifyErr
    = InternalDuplicateGenIdentErr (A Ident 'Resolved) (IdentBind 'Resolved)
    | InternalUnresolvedGenIdentErr (A Ident 'Resolved)

instance ErrMessage InternalSimplifyErr where
    errMessage (InternalDuplicateGenIdentErr new old) =
        [ "Duplicate generated identifier " ++ formatQuote new
        , formatQuote old ++ " is already defined" ]
    errMessage (InternalUnresolvedGenIdentErr ident) =
        [ "Unresolved generated identifier " ++ formatQuote ident ]
