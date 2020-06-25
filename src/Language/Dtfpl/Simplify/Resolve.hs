{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Resolve identifiers.
module Language.Dtfpl.Simplify.Resolve
    ( resolve
    ) where

import           Control.Monad
import           Data.Bifunctor
import qualified Data.Map.Strict                     as M
import           Data.Singletons.Prelude.Enum
import           Numeric.Natural
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err
import           Language.Dtfpl.Simplify.SimplifyErr
import           Language.Dtfpl.Step
import           Language.Dtfpl.Syntax
import           Language.Dtfpl.Syntax.Util
import           Language.Dtfpl.Util

data Name
    = Name String
    | GenNamePart Name Natural
    | GenNameFull Natural
    deriving (Eq, Ord)

identToName :: Ident 'Resolved -> Name
identToName (Ident s)                    = Name s
identToName (GenIdentPart (A i _) (P n)) = GenNamePart (identToName i) n
identToName (GenIdentFull (P n))         = GenNameFull n

identBindToName :: IdentBind 'Resolved -> Name
identBindToName (IdentBind (A ident _)) = identToName ident

type NameMap = M.Map Name (IdentBind 'Resolved)

type GlobalEffs = '[Reader Config, Error Err]

type instance StepEffs 'Resolved = Reader NameMap ': GlobalEffs

resolve :: Members GlobalEffs r
    => A Prog (Pred 'Resolved) -> Sem r (A Prog 'Resolved)
resolve = runReader M.empty . step

instance Step (T [] (A TopLevel)) 'Resolved where
    step (T tls) = do
        let (binds, bodies) = unzip $ map splitLet tls
              where splitLet (A (TLDecl _ (A decl _)) _) = case decl of
                        Let bind body -> (bind, body)
                        Def bind _    -> absurdP bind
            stepBinds :: Members (StepEffs 'Resolved) r
                => [IdentBind (Pred 'Resolved)]
                -> Sem r ([IdentBind 'Resolved],
                    M.Map Name (IdentBind 'Resolved))
            stepBinds [] = ([] ,) <$> ask
            stepBinds (ib:ibs) = do
                sib <- step ib
                first (sib :) <$>
                    local (M.insert (identBindToName sib) sib) (stepBinds ibs)
        (sBinds, names) <- stepBinds binds
        sBodies <- local (M.union names) $ traverse step bodies
        pure $ T $ zipWith3 (mapNode .: mapTLDecl .: mapNode .: const .: Let)
            sBinds sBodies tls

instance Step IdentBind 'Resolved where
    step (IdentBind ident) = do
        si <- step ident
        let sni = node si
            checkDupWithErr errCtor =
                asks (M.lookup $ identToName sni) >>= \case
                    Nothing -> pure ()
                    Just old -> throw $ errCtor si old
            checkGenDup = do
                d <- asks debug
                when d $ checkDupWithErr $ InternalErr
                    .: InternalSimplifyErr .: InternalDuplicateGenIdentErr
        case sni of
            Ident _ -> checkDupWithErr $ SimplifyErr .: DuplicateIdentErr
            GenIdentPart _ _ -> checkGenDup
            GenIdentFull _ -> checkGenDup
        pure $ IdentBind si
