{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Resolve identifiers.
module Language.Dtfpl.Simplify.Resolve
    ( resolve
    ) where

import Capability.Reader
import Capability.Error
import qualified Data.Map.Strict                     as M
import           Data.Singletons.Prelude.Enum
import           Numeric.Natural
import Data.Bifunctor
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad

import Data.Traversable

import           Language.Dtfpl.Err
import           Language.Dtfpl.M
import           Language.Dtfpl.Simplify.SimplifyErr
import           Language.Dtfpl.Step
import           Language.Dtfpl.Syntax
import           Language.Dtfpl.Syntax.Util
import           Language.Dtfpl.Util
import Language.Dtfpl.M.Util
import Language.Dtfpl.Config

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

type MResolve = HasReader "names" NameMap

newtype ResolveT m a = ResolveT { runResolveT :: ReaderT NameMap m a }
    deriving (Functor, Applicative, Monad)
    deriving MResolve via MonadReader (ReaderT NameMap m)

type instance StepClass' 'Resolved m = (MResolve m, MConfig m, MError m)

resolve :: (Monad m, MConfig (ResolveT m), MError (ResolveT m)) => A Prog (Pred 'Resolved) -> m (A Prog 'Resolved)
resolve prog = runReaderT (runResolveT $ step prog) M.empty

instance Step (T [] (A TopLevel)) 'Resolved where
    step (T tls) = do
        let (binds, bodies) = unzip $ map splitLet tls
              where splitLet (A (TLDecl _ (A decl _)) _) = case decl of
                        Let bind body -> (bind, body)
                        Def bind _ -> absurdP bind
            stepBinds [] = ([] ,) <$> ask @"names"
            stepBinds (ib:ibs) = do
                sib <- step ib
                first (sib :) <$> local @"names" (M.insert (identBindToName sib) sib) (stepBinds ibs)
        (sBinds, names) <- stepBinds binds

        undefined

instance Step IdentBind 'Resolved where
    step (IdentBind ident) = do
        si <- step ident
        let sni = node si
            checkDupWithErr errCtor =
                asks @"names" (M.lookup $ identToName sni) >>= \case
                    Nothing -> pure ()
                    Just old -> throw @"err" $ errCtor si old
            checkGenDup = do
                d <- asks @"config" debug
                when d $ checkDupWithErr $ InternalErr
                    .: InternalSimplifyErr .: InternalDuplicateGenIdentErr
        case sni of
            Ident _ -> checkDupWithErr $ SimplifyErr .: DuplicateIdentErr
            GenIdentPart _ _ -> checkGenDup
            GenIdentFull _ -> checkGenDup
        pure $ IdentBind si
