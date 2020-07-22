{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Dtfpl.Simplify.ParseNative () where

import           Data.Aeson
import           GHC.Generics
import           Polysemy.Error
import           Text.Megaparsec.Pos

import           Language.Dtfpl.Err
import           Language.Dtfpl.NodeProc
import           Language.Dtfpl.Parse.Loc
import           Language.Dtfpl.Simplify.SimplifyErr
import           Language.Dtfpl.Step
import           Language.Dtfpl.Syntax
import           Language.ECMAScript.Syntax          hiding (Member)

type instance StepEffs 'ParsedNative = '[Error Err, NodeProc]

data AcornPos = AcornPos
    { line   :: Int
    , column :: Int }
    deriving (Generic, FromJSON)

data ParseNativeResult
    = ParseNativeSuccess Value
    | ParseNativeError String AcornPos
    deriving (Generic, FromJSON)

data ParseNative
instance Message ParseNative String ParseNativeResult

instance Step Native 'ParsedNative where
    step (Native (A (P str) loc)) = send' @ParseNative str >>= \case
            ParseNativeSuccess expr -> pure $
                Native $ A (P $ PassthruExpression expr) loc
            ParseNativeError msg acornPos ->
                let startPos@SourcePos {..} = start loc
                    pos = case line acornPos of
                        1 -> startPos
                            { sourceColumn = case column acornPos of
                                0 -> sourceColumn
                                c -> sourceColumn <> mkPos c }
                        l -> startPos
                            { sourceLine = sourceLine <> mkPos (pred l)
                            , sourceColumn = mkPos $ succ $ column acornPos }
                in  throw $ SimplifyErr $ ParseNativeErr msg pos
