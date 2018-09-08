{-# LANGUAGE DataKinds #-}

module Language.Dtfpl.ParseNative
    ( parseNative
    ) where

import Language.Dtfpl.Syntax

parseNative :: A Prog 'Source -> IO (A Prog 'ParsedNative)
parseNative = undefined
