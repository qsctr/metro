{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Language.Dtfpl.Module.ModuleErr
    ( ModuleErr (..)
    ) where

import           Data.List.NonEmpty            (NonEmpty)
import qualified Data.List.NonEmpty            as N

import           Language.Dtfpl.Err.ErrLoc
import           Language.Dtfpl.Err.ErrMessage
import           Language.Dtfpl.Format
import           Language.Dtfpl.Format.Util
import           Language.Dtfpl.Syntax

data ModuleErr
    = UnresolvedModuleErr (A (P ModName) 'Source)
    | CyclicImportErr
        (NonEmpty (A Import 'ModResolved))
            -- ^ "Import stack" containing the cycle
            -- The annotation location of the last import should be in the same
            -- file as the resolved path of the first import

instance ErrMessage ModuleErr where
    errMessage (UnresolvedModuleErr modName) =
        [ "Could not find module " ++ format modName ]
    {-
    A (Import a.dtfpl a) (b.dtfpl pos)
    A (Import b.dtfpl b) (c.dtfpl pos)
    A (Import c.dtfpl c) (d.dtfpl pos)
    ...
    A (Import y.dtfpl y) (z.dtfpl pos)
    A (Import z.dtfpl z) (a.dtfpl pos)

    becomes

    module a (a.dtfpl)
    is imported by module b (at b.dtfpl pos) which
    is imported by module c (at c.dtfpl pos) which
    ...
    is imported by module z (at z.dtfpl pos) which
    is imported by module a (at a.dtfpl pos)
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
    errLoc (CyclicImportErr imps)        = Just $ ann $ N.head imps
