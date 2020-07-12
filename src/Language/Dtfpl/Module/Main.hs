{-# LANGUAGE TypeFamilies #-}

module Language.Dtfpl.Module.Main
    ( mainFn
    , hasMainFn
    ) where

import           Data.Void
import           Numeric.Natural

import           Language.Dtfpl.Parse.Loc
import           Language.Dtfpl.Syntax
import           Language.Dtfpl.Syntax.Util

mainFn :: Ident p
mainFn = Ident "main"

hasMainFn ::
    ( GenIdentPartPrefix p ~ A Ident
    , GenIdentPartNum p ~ P Natural
    , DefHead p ~ P Void
    , Ann p ~ Maybe Loc
    ) => [A TopLevel p] -> Bool -- why do I need all these constraints...
hasMainFn tls = mainFn `elem` map (node . unIdentBind) (getTLBinds tls)
