{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Resolve identifiers.
module Language.Dtfpl.Simplify.NameResolve
    ( nameResolve
    ) where

import           Data.Bifunctor
import           Data.List.NonEmpty                  (NonEmpty (..))
import qualified Data.List.NonEmpty                  as N
import           Data.Map.Strict                     (Map)
import qualified Data.Map.Strict                     as M
import           Data.Singletons.Prelude.Enum
import           Numeric.Natural
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err
import           Language.Dtfpl.Interface.Syntax
import           Language.Dtfpl.Module.Deps
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

type CanIdentToName p =
    ( GenIdentPartPrefix p ~ A Ident
    , GenIdentPartNum p ~ P Natural
    , GenIdentFullNum p ~ P Natural )

identToName :: CanIdentToName p => Ident p -> Name
identToName (Ident s)                    = Name s
identToName (GenIdentPart (A i _) (P n)) = GenNamePart (identToName i) n
identToName (GenIdentFull (P n))         = GenNameFull n

identBindToName :: CanIdentToName p => IdentBind p -> Name
identBindToName (IdentBind (A ident _)) = identToName ident

type NameMap = Map Name NameMapValue
type NameMapValue = Either (NonEmpty ImpIdentBind) (IdentBind 'NameResolved)

type GlobalEffs = '[Reader Config, Error Err]

type instance StepEffs 'NameResolved = Reader NameMap ': GlobalEffs

nameResolve :: Members (Reader ModuleDeps ': GlobalEffs) r
    => A Mod (Pred 'NameResolved) -> Sem r (A Mod 'NameResolved)
nameResolve mod_ = do
    deps <- ask
    let depNameMap = M.map Left $ M.fromListWith (<>)
            [ (identBindToName bind, ImpIdentBind modName bind :| [])
            | (modName, IMod binds) <- M.toList deps, bind <- binds ]
    runReader depNameMap $ step mod_

addName :: Member (Reader NameMap) r
    => IdentBind 'NameResolved -> Sem r a -> Sem r a
addName sib = local $ M.insert (identBindToName sib) (Right sib)

stepBinds :: (Step n 'NameResolved, Members (StepEffs 'NameResolved) r)
    => [n (Pred 'NameResolved)]
    -> (n 'NameResolved -> Maybe (IdentBind 'NameResolved))
    -> ([n 'NameResolved] -> Sem r a) -> Sem r a
stepBinds xxs getIdentBind cont = do
    (sxxs, names) <- go xxs
    local (const names) $ cont sxxs
  where go [] = ([] ,) <$> ask @NameMap
        go (x:xs) = do
            sx <- step x
            first (sx :) <$> case getIdentBind sx of
                Just sib -> addName sib $ go xs
                Nothing  -> go xs

instance Step (T [] (A TopLevel)) 'NameResolved where
    step (T tls) = stepBinds binds Just \sBinds -> do
        sBodies <- traverse step bodies
        pure $ T $ zipWith3 (mapNode .: mapTLDecl .: mapNode .: const .: Let)
            sBinds sBodies tls
      where (binds, bodies) = unzip $ map splitTLDecl tls

instance Step CaseAlt 'NameResolved where
    step (CaseAlt (T pats) expr) = stepBinds (N.toList pats)
        \case
            A (VarPat sib) _ -> Just sib
            _ -> Nothing
        \sPats -> CaseAlt (T $ N.fromList sPats) <$> step expr

instance Step Lam 'NameResolved where
    step (Lam ib expr) = do
        sib <- step ib
        Lam sib <$> addName sib (step expr)

lookupIdent :: Member (Reader NameMap) r
    => A Ident 'NameResolved -> Sem r (Maybe NameMapValue)
lookupIdent = asks . M.lookup . identToName . node

instance Step IdentBind 'NameResolved where
    step (IdentBind ident) = do
        si <- step ident
        let checkDupWithErr errCtor = lookupIdent si >>= \case
                Nothing -> pure ()
                Just old -> throw $ errCtor si old
        if isSourceIdent $ node si
            then checkDupWithErr $ SimplifyErr .: DuplicateIdentErr
            else whenM (asks debug) $ checkDupWithErr $ InternalErr
                    .: InternalSimplifyErr .: InternalDuplicateGenIdentErr
        pure $ IdentBind si

instance Step IdentRef 'NameResolved where
    step (IdentRef U ident) = do
        si <- step ident
        lookupIdent si >>= \case
            Nothing -> throw
                if isSourceIdent $ node si
                    then SimplifyErr $ UnresolvedIdentErr si
                    else InternalErr $
                        InternalSimplifyErr $ InternalUnresolvedGenIdentErr si
            Just entry -> case entry of
                Left (iib :| iibs) -> case N.nonEmpty iibs of
                    Nothing -> pure $ IdentRef (T $ Left iib) si
                    Just iibs' -> throw $
                        SimplifyErr $ AmbiguousIdentErr si iib iibs'
                Right ib -> pure $ IdentRef (T $ Right ib) si
