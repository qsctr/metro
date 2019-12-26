{-# LANGUAGE DataKinds #-}

-- | Errors for simplify phase
module Language.Dtfpl.Simplify.SimplifyErr
    ( SimplifyErr (..)
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

instance ErrMessage SimplifyErr where
    errMessage (DuplicateIdentErr new old) =
        [ "Duplicate identifier " ++ formatQuote new
        , case ann $ unIdentBind old of
            Nothing -> formatQuote old ++ " is already defined"
            Just loc -> "Previously defined " ++ formatQuote old
                ++ " at " ++ formatLoc loc ]

instance ErrLoc SimplifyErr where
    errLoc (DuplicateIdentErr new _) = ann new