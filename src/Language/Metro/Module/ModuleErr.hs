{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Language.Metro.Module.ModuleErr
    ( ModuleErr (..)
    ) where

import           Data.List.NonEmpty            (NonEmpty)
import qualified Data.List.NonEmpty            as N

import           Language.Metro.Err.ErrLoc
import           Language.Metro.Err.ErrMessage
import           Language.Metro.Format
import           Language.Metro.Format.Util
import           Language.Metro.Syntax
import           Language.Metro.Util.EPath

data ModuleErr
    = UnresolvedModuleErr (A (P ModName) 'Source)
    | LoadModuleErr
        EFile -- ^ The module to load
        IOError -- ^ The error
    | CyclicImportErr
        (NonEmpty (A Import 'ModResolved))
            -- ^ "Import stack" containing the cycle
            -- The annotation location of the last import should be in the same
            -- file as the resolved path of the first import

instance ErrMessage ModuleErr where
    errMessage (UnresolvedModuleErr modName) =
        [ "Could not find module " ++ format modName ]
    errMessage (LoadModuleErr path ioe) =
        [ "Could not load module " ++ format path
        , show ioe ]
    {-
    A (Import a.metro a) (b.metro pos)
    A (Import b.metro b) (c.metro pos)
    A (Import c.metro c) (d.metro pos)
    ...
    A (Import y.metro y) (z.metro pos)
    A (Import z.metro z) (a.metro pos)

    becomes

    module a (a.metro)
    is imported by module b (at b.metro pos) which
    is imported by module c (at c.metro pos) which
    ...
    is imported by module z (at z.metro pos) which
    is imported by module a (at a.metro pos)
    -}
    errMessage (CyclicImportErr imps) =
        "Cyclic import" : firstMsg : midMsgs ++ [lastMsg]
      where firstMsg = "module " ++ format modName ++ formatParens path
              where Import path modName = node $ N.head imps
            midMsgs = map (++ " which") $
                zipWith restMsg (N.tail imps) (N.toList imps)
            lastMsg = restMsg (N.head imps) (N.last imps)
            restMsg (A (Import _ modName) _) (A _ impLoc) =
                "  is imported by module " ++ format modName
                ++ parens ("at " ++ format impLoc)

instance ErrLoc ModuleErr where
    errLoc (UnresolvedModuleErr modName) = Just $ ann modName
    errLoc (LoadModuleErr _ _)           = Nothing
    errLoc (CyclicImportErr imps)        = Just $ ann $ N.head imps
