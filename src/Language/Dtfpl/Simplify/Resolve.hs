{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TupleSections #-}

{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Resolve identifiers.
module Language.Dtfpl.Simplify.Resolve
    ( resolve
    ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.Map.Strict                     as M
import           Data.Singletons.Prelude.Enum
import           Numeric.Natural
import Data.Bifunctor

import Data.Traversable

import           Language.Dtfpl.Err
import           Language.Dtfpl.M
import           Language.Dtfpl.Simplify.SimplifyErr
import           Language.Dtfpl.Step
import           Language.Dtfpl.Syntax
import           Language.Dtfpl.Syntax.Util
import           Language.Dtfpl.Util
import Language.Dtfpl.M.Util

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

-- type MResolve = MonadReader (M.Map Name (IdentBind 'Resolved))

class Monad m => MResolve m where
    getNames :: m (M.Map Name (IdentBind 'Resolved))
    lookupName :: Ident 'Resolved -> m (Maybe (IdentBind 'Resolved))
    addName :: IdentBind 'Resolved -> m a -> m a

instance Monad m => MResolve (ReaderT (M.Map Name (IdentBind 'Resolved)) m) where
    getNames = ask
    lookupName = asks . M.lookup . identToName
    addName ib = local (M.insert (identBindToName ib) ib)

type instance StepClass' 'Resolved m = ()

resolve :: Monad m => A Prog (Pred 'Resolved) -> m (A Prog 'Resolved)
resolve = step

-- type instance StepClass' 'Resolved m = (MResolve m, MEnv m, MError m)

-- resolve :: (MEnv m, MError m) => A Prog (Pred 'Resolved) -> m (A Prog 'Resolved)
-- resolve prog = runReaderT (step prog) M.empty

-- instance Step (T [] (A TopLevel)) 'Resolved where
--     step (T tls) = do
--         let (binds, bodies) = unzip $ map splitLet tls
--               where splitLet (A (TLDecl _ (A decl _)) _) = case decl of
--                         Let bind body -> (bind, body)
--                         Def bind _ -> absurdP bind
--             stepBinds [] = ([] ,) <$> getNames
--             stepBinds (ib:ibs) = do
--                 sib <- step ib
--                 first (sib :) <$> addName sib (stepBinds ibs)
--         (sBinds, names) <- stepBinds binds

--         undefined

-- instance Step IdentBind 'Resolved where
--     step (IdentBind ident) = do
--         si <- step ident
--         let sni = node si
--             checkDupWithErr errCtor =
--                 lookupName sni >>= \case
--                     Nothing -> pure ()
--                     Just old -> throwError $ errCtor si old
--             checkGenDup = do
--                 d <- getDebug
--                 when d $ checkDupWithErr $ InternalErr
--                     .: InternalSimplifyErr .: InternalDuplicateGenIdentErr
--         case sni of
--             Ident _ -> checkDupWithErr $ SimplifyErr .: DuplicateIdentErr
--             GenIdentPart _ _ -> checkGenDup
--             GenIdentFull _ -> checkGenDup
--         pure $ IdentBind si
