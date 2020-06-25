{-# LANGUAGE DataKinds #-}

-- | Errors for simplify phase
module Language.Dtfpl.Simplify.SimplifyErr
    ( SimplifyErr (..)
    , InternalSimplifyErr (..)
    ) where

import           Language.Dtfpl.Err.ErrLoc
import           Language.Dtfpl.Err.ErrMessage
import           Language.Dtfpl.Format.Util
import           Language.Dtfpl.Parser.Loc
import           Language.Dtfpl.Syntax

-- | Error during simplify phase
data SimplifyErr
    -- | An identifier being defined with the same name as an already existing
    -- one.
    = DuplicateIdentErr
        (A Ident 'Resolved)   -- ^ The duplicate identifier
        (IdentBind 'Resolved) -- ^ The original identifier
    -- | Unresolved identifier.
    | UnresolvedIdentErr (A Ident 'Resolved)

instance ErrMessage SimplifyErr where
    errMessage (DuplicateIdentErr new old) =
        [ "Duplicate identifier " ++ formatQuote new
        , case ann $ unIdentBind old of
            Nothing -> formatQuote old ++ " is already defined"
            Just loc -> "Previously defined " ++ formatQuote old
                ++ " at " ++ formatLoc loc ]
    errMessage (UnresolvedIdentErr ident) =
        [ "Unresolved identifier " ++ formatQuote ident ]

instance ErrLoc SimplifyErr where
    errLoc (DuplicateIdentErr new _) = ann new
    errLoc (UnresolvedIdentErr ident) = ann ident

data InternalSimplifyErr
    = InternalDuplicateGenIdentErr (A Ident 'Resolved) (IdentBind 'Resolved)
    | InternalUnresolvedGenIdentErr (A Ident 'Resolved)

instance ErrMessage InternalSimplifyErr where
    errMessage (InternalDuplicateGenIdentErr new old) =
        [ "Duplicate generated identifier " ++ formatQuote new
        , formatQuote old ++ " is already defined" ]
    errMessage (InternalUnresolvedGenIdentErr ident) =
        [ "Unresolved generated identifier " ++ formatQuote ident ]
