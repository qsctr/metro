{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeFamilies     #-}

module Language.Dtfpl.Module.Resolve
    ( resolveModule
    ) where

import qualified Data.List.NonEmpty              as N
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           System.Path                     ((</>))
import qualified System.Path                     as P

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err
import           Language.Dtfpl.Module.Load
import           Language.Dtfpl.Module.ModuleErr
import           Language.Dtfpl.Syntax
import           Language.Dtfpl.Util.EPath

resolveModule :: Members '[LoadModule, Reader Config, Error Err] r
    => A (P ModName) 'Source -> Sem r EFile
resolveModule modName = asks moduleSearchPaths >>= go
  where go [] = throw $ ModuleErr $ UnresolvedModuleErr modName
        go (EPath dir : dirs) = do
            let file = dir </> modNamePath
            exists <- moduleExists file
            if exists
                then pure $ EPath file
                else go dirs
        modNamePath = P.joinPath $ map unModAtom $ N.toList $ atoms
        A (P (ModName atoms)) _ = modName
