{-# LANGUAGE BlockArguments        #-}
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
module Language.Dtfpl.Simplify.Resolve
    ( resolve
    ) where

import           Control.Monad
import           Data.Bifunctor
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

type NameMap = Map Name (IdentBind 'Resolved)

type GlobalEffs = '[Reader Config, Error Err]

type instance StepEffs 'Resolved = Reader NameMap ': GlobalEffs

resolve :: Members GlobalEffs r
    => A Prog (Pred 'Resolved) -> Sem r (A Prog 'Resolved)
resolve = runReader M.empty . step

stepBinds :: (Step n 'Resolved, Members (StepEffs 'Resolved) r)
    => [n (Pred 'Resolved)] -> (n 'Resolved -> Maybe (IdentBind 'Resolved))
    -> ([n 'Resolved] -> Sem r a) -> Sem r a
stepBinds xxs getIdentBind cont = do
    (sxxs, names) <- go xxs
    local (const names) $ cont sxxs
  where go [] = ([] ,) <$> ask @NameMap
        go (x:xs) = do
            sx <- step x
            first (sx :) <$> case getIdentBind sx of
                Just sib -> local (M.insert (identBindToName sib) sib) $ go xs
                Nothing -> go xs

instance Step (T [] (A TopLevel)) 'Resolved where
    step (T tls) = stepBinds binds Just \sBinds -> do
        sBodies <- traverse step bodies
        pure $ T $ zipWith3 (mapNode .: mapTLDecl .: mapNode .: const .: Let)
            sBinds sBodies tls
      where (binds, bodies) = unzip $ map splitLet tls
            splitLet (A (TLDecl _ (A decl _)) _) = case decl of
                Let bind body -> (bind, body)
                Def bind _    -> absurdP bind

instance Step CaseAlt 'Resolved where
    step (CaseAlt (T pats) expr) = stepBinds (N.toList pats)
        \case
            A (VarPat sib) _ -> Just sib
            _ -> Nothing
        \sPats -> CaseAlt (T $ N.fromList sPats) <$> step expr

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
