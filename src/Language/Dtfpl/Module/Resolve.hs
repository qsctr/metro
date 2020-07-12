{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Dtfpl.Module.Resolve
    ( resolveImports
    ) where

import qualified Data.List.NonEmpty              as N
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import           System.Path                     ((<.>), (</>))
import qualified System.Path                     as P

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err
import           Language.Dtfpl.Module.ModuleErr
import           Language.Dtfpl.Step
import           Language.Dtfpl.Syntax
import           Language.Dtfpl.Util.EPath
import           Language.Dtfpl.Util.FS

type instance StepEffs 'ModResolved = '[Reader Config, Error Err, FS]

resolveImports :: Members (StepEffs 'ModResolved) r
    => A Mod 'Source -> Sem r (A Mod 'ModResolved)
resolveImports = step

instance Step Import 'ModResolved where
    step (Import U modName@(A (P (ModName atoms)) _)) =
        asks moduleSearchPaths >>= go
      where go [] = throw $ ModuleErr $ UnresolvedModuleErr modName
            go (EPath dir : dirs) = do
                let file = dir </> modNamePath <.> "dtfpl"
                exists <- fsFileExists file
                if exists
                    then Import (P $ EPath file) <$> step modName
                    else go dirs
            modNamePath = P.joinPath $ map unModAtom $ N.toList atoms
