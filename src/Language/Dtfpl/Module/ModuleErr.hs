{-# LANGUAGE DataKinds #-}

module Language.Dtfpl.Module.ModuleErr
    ( ModuleErr (..)
    ) where

import Language.Dtfpl.Syntax
import Language.Dtfpl.Err.ErrMessage
import Language.Dtfpl.Err.ErrLoc
import Language.Dtfpl.Format

data ModuleErr
    = UnresolvedModuleErr (A (P ModName) 'Source)

instance ErrMessage ModuleErr where
    errMessage (UnresolvedModuleErr modName) =
        [ "Could not find module " ++ format modName ]

instance ErrLoc ModuleErr where
    errLoc (UnresolvedModuleErr modName) = Just $ ann modName
