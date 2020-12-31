{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

module Language.Metro.Util.EPath
    ( EPath (..)
    , EFile
    , EDir
    , replaceExt
    ) where

import qualified System.Path           as P
import qualified System.Path.Part      as PP
import qualified System.Path.PartClass as PC

-- | 'P.File' with the ar type variable existentially quantified.
data EPath fd = forall ar. PC.AbsRel ar => EPath (P.Path ar fd)

type EFile = EPath PP.File
type EDir = EPath PP.Dir

replaceExt :: String -> EFile -> EFile
replaceExt ext (EPath path) = EPath $ P.replaceExtension path ext
