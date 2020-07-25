{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ViewPatterns     #-}

-- | Errors for simplify phase
module Language.Dtfpl.Simplify.SimplifyErr
    ( SimplifyErr (..)
    , InternalSimplifyErr (..)
    ) where

import           Data.List.NonEmpty            (NonEmpty (..))
import qualified Data.List.NonEmpty            as N
import           Data.Singletons.Prelude.Enum
import           Text.Megaparsec.Pos

import           Language.Dtfpl.Err.ErrLoc
import           Language.Dtfpl.Err.ErrMessage
import           Language.Dtfpl.Format
import           Language.Dtfpl.Format.Util
import           Language.Dtfpl.Parse.Loc
import           Language.Dtfpl.Syntax
import           Language.Dtfpl.Syntax.Util

-- | Error during simplify phase
data SimplifyErr
    = ParseNativeErr
        String -- ^ Error message
        SourcePos -- ^ Error position
    | InvalidNativeArgErr (IdentBind (Pred 'CheckedNativeParams))
    -- | An identifier being defined with the same name as an already existing
    -- one.
    | DuplicateIdentErr
        (A Ident 'NameResolved) -- ^ The duplicate identifier
        (Either (NonEmpty ImpIdentBind) (IdentBind 'NameResolved))
            -- ^ The original identifier(s)
    -- | Unresolved identifier.
    | UnresolvedIdentErr (A Ident 'NameResolved)
    -- | Ambiguous identifier. Note that the possible identifiers it could refer
    -- to must be imported since identifiers within the current module are
    -- unique (and would otherwise cause a 'DuplicateIdentErr').
    | AmbiguousIdentErr
        (A Ident 'NameResolved) -- ^ The ambiguous reference
        ImpIdentBind -- ^ The first identifier it could refer to
        (NonEmpty ImpIdentBind)
            -- ^ The rest of the identifiers that it could refer to
    -- | Cyclic declarations.
    | CyclicDeclErr
        (A Decl (Pred 'Reordered)) -- ^ The declaration causing the cycle
        (NonEmpty (A Decl (Pred 'Reordered))) -- ^ "Call stack" of declarations

instance ErrMessage SimplifyErr where
    errMessage (ParseNativeErr msg pos) =
        [ "Error while parsing native code"
        , format pos
        , msg ]
    errMessage (InvalidNativeArgErr ident) =
        [ "Invalid native function parameter name " ++ formatQuote ident
        , "Native function parameter names must be valid ECMAScript identifiers"
        , "  and must not contain `$` or `_`" ]
    errMessage (DuplicateIdentErr new old) =
        ("Duplicate identifier " ++ formatQuote new)
        : duplicateIdentMessage old
    errMessage (UnresolvedIdentErr ident) =
        [ "Unresolved identifier " ++ formatQuote ident ]
    errMessage (AmbiguousIdentErr ident iib iibs) =
        [ "Ambiguous identifier " ++ formatQuote ident
        , "It could refer to " ++ msg iib ]
        ++ map (("  or to " ++) . msg) (N.toList iibs)
      where msg (ImpIdentBind modName ib) = formatQuote ib
                ++ " defined in module " ++ format modName
                ++ parensLoc (ann $ unIdentBind ib)
    errMessage (CyclicDeclErr d ds) =
        "Cyclic declaration" : reverse (lastMsg : msg ds)
      where lastMsg = depends d ++ " again"
            msg (N.uncons -> (x, rest)) = case rest of
                Nothing -> ["The evaluation of " ++ nameLoc x]
                Just xs -> (depends x ++ " which") : msg xs
            depends x = "  depends on " ++ nameLoc x
            nameLoc :: A Decl (Pred 'Reordered) -> String
            nameLoc (A decl a) = case decl of
                Let bind _ -> formatQuote bind ++ parensLoc a
                Def bind _ -> absurdP bind

instance ErrLoc SimplifyErr where
    errLoc (ParseNativeErr _ _)          = Nothing
    errLoc (InvalidNativeArgErr ib)      = Just $ ann $ unIdentBind ib
    errLoc (DuplicateIdentErr new _)     = ann new
    errLoc (UnresolvedIdentErr ident)    = ann ident
    errLoc (AmbiguousIdentErr ident _ _) = ann ident
    errLoc (CyclicDeclErr decl _)        = ann decl

data InternalSimplifyErr
    = InternalDuplicateGenIdentErr
        (A Ident 'NameResolved)
        (Either (NonEmpty ImpIdentBind) (IdentBind 'NameResolved))
    | InternalUnresolvedGenIdentErr (A Ident 'NameResolved)

instance ErrMessage InternalSimplifyErr where
    errMessage (InternalDuplicateGenIdentErr new old) =
        ("Duplicate generated identifier " ++ formatQuote new)
        : duplicateIdentMessage old
    errMessage (InternalUnresolvedGenIdentErr ident) =
        [ "Unresolved generated identifier " ++ formatQuote ident ]

duplicateIdentMessage :: (Format (IdentBind p), Ann p ~ Maybe Loc)
    => Either (NonEmpty ImpIdentBind) (IdentBind p) -> [String]
duplicateIdentMessage = \case
    Left (iib :| iibs) -> firstMsg iib : map restMsg iibs
    Right ib -> [alreadyDefined ib ++ at ib]
  where firstMsg iib@(ImpIdentBind _ ib) = alreadyDefined ib ++ info iib
        restMsg riib = "  and" ++ info riib
        info (ImpIdentBind modName ib) =
            " in module " ++ format modName ++ at ib
        alreadyDefined bind = formatQuote bind ++ " is already defined"
        at (IdentBind ident) = case ann ident of
            Nothing  -> ""
            Just loc -> " at " ++ format loc
