{-# LANGUAGE TypeFamilies #-}

module Language.Metro.Module.Main
    ( mainFn
    , hasMainFn
    ) where

import           Data.Void
import           Numeric.Natural

import           Language.Metro.Parse.Loc
import           Language.Metro.Syntax
import           Language.Metro.Syntax.Util

mainFn :: Ident p
mainFn = Ident "main"

hasMainFn ::
    ( GenIdentPartPrefix p ~ A Ident
    , GenIdentPartNum p ~ P Natural
    , DefHead p ~ P Void
    , Ann p ~ Maybe Loc
    ) => [A TopLevel p] -> Bool -- why do I need all these constraints...
hasMainFn tls = mainFn `elem` map (node . unIdentBind) (getTLBinds tls)
