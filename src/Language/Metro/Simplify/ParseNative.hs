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

module Language.Metro.Simplify.ParseNative () where

import           Data.Aeson
import           GHC.Generics
import           Polysemy.Error
import           Text.Megaparsec.Pos

import           Language.Metro.Err
import           Language.Metro.NodeProc
import           Language.Metro.Parse.Loc
import           Language.Metro.Simplify.SimplifyErr
import           Language.Metro.Step
import           Language.Metro.Syntax
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
