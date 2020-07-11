{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Generate JS from Dtfpl Core
module Language.Dtfpl.Generate
    ( generate
    ) where

import qualified Data.List.NonEmpty                as N
import           Data.Traversable
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader

import           Language.Dtfpl.Config
import           Language.Dtfpl.Err
import           Language.Dtfpl.Syntax
import           Language.Dtfpl.Syntax.Util
import           Language.Dtfpl.Util
import           Language.ECMAScript.Syntax
import           Language.ECMAScript.Syntax.Util
import           Language.ECMAScript.Syntax.Verify

generate :: Members '[Reader Config, Error Err] r => A Mod Core -> Sem r Program
generate = toJS

-- | Typeclass for dtfpl core node @n@ which can be converted to JS node @js@.
class ToJS n js where
    -- | Convert a dtfpl core node to a js node.
    toJS :: Members '[Reader Config, Error Err] r => n Core -> Sem r js

-- | Currently we just throw away the annotations.
-- Might add source maps later on.
instance ToJS n js => ToJS (A n) js where
    toJS (A n _) = toJS n

instance ToJS Mod Program where
    toJS (Mod (T _) (T tls)) = Program ModuleSourceType <$> ((++)
        <$> traverse toJS tls
        <*> (pure . Left' . ExpressionStatement <$> (CallExpression
            <$> (Left' . IdentifierExpression <$> mkIdentifierE "main")
            <*> (pure . Left' <$> (CallExpression
                <$> (Left' . MemberExpression <$> (Member
                    <$> (Left' . MemberExpression <$> (Member
                        <$> (Left' . IdentifierExpression
                            <$> mkIdentifierE "process")
                        <*> (Right' <$> mkIdentifierE "argv")))
                    <*> (Right' <$> mkIdentifierE "slice")))
                <*> pure [Left' (LiteralExpression (NumberLiteral 2))])))))

instance ToJS Import ModuleDeclaration where
    toJS (Import _) = undefined -- TODO

instance ToJS TopLevel (Either' Statement ModuleDeclaration) where
    toJS (TLDecl Exp decl) =
        Right' . ExportNamedDeclarationDeclaration <$> toJS decl
    toJS (TLDecl Priv decl) = Left' . DeclarationStatement <$> toJS decl

instance ToJS Decl Declaration where
    toJS (Let ident expr) = constDecl <$> toJS ident <*> toJS expr
    toJS (Def _ _)        = undefined

-- | For the 'Case' expression:
-- Converts each 'Case' into an IIFE where inside the function body there are
-- if statements corresponding to each pattern match.
instance ToJS Expr Expression where
    toJS (VarExpr ident) = IdentifierExpression <$> toJS ident
    toJS (LitExpr lit) = LiteralExpression <$> toJS lit
    toJS (App f x) = CallExpression
        <$> (Left' <$> toJS f) <*> (pure . Left' <$> toJS x)
    toJS (If cond true false) = ConditionalExpression
        <$> toJS cond <*> toJS true <*> toJS false
    toJS (Case (CaseHead (T aliExprs)) (T alts)) = do
        let (aliases, headExprs) = mapAccumR
                (\alis (AliExpr expr (T ali)) -> case ali of
                    Nothing -> (alis, expr)
                    Just ident -> ((ident, expr) : alis,
                        genLoc $ VarExpr $ identBindToRef ident))
                [] aliExprs
        aliStmts <- for aliases $ \(ident, expr) ->
            DeclarationStatement .: constDecl <$> toJS ident <*> toJS expr
        headExprs' <- traverse toJS headExprs
        altStmts <- for alts $ \(A (CaseAlt (T pats) altExpr) _) -> do
            let (checkPairs, nonCheckPairs) =
                    N.partition (needsCheck . node . snd) $
                        N.zip headExprs' pats
                  where needsCheck (VarPat _) = False
                        needsCheck (LitPat _) = True
                        needsCheck WildPat    = False
            binds <- forMaybe nonCheckPairs $ \(headExpr, A pat _) ->
                case pat of
                    VarPat ident -> Just $
                        DeclarationStatement . flip constDecl headExpr
                            <$> toJS ident
                    WildPat -> Nothing
                    _ -> undefined
            ret <- ReturnStatement . Just <$> toJS altExpr
            let body = BlockStatement $ Block $ binds ++ [ret]
            case checkPairs of
                [] -> pure body
                _ -> do
                    conds <- for checkPairs $ \(headExpr, A pat _) ->
                        case pat of
                            LitPat lit ->
                                BinaryExpression StrictEqualOperator headExpr
                                . LiteralExpression <$> toJS lit
                            _ -> undefined
                    pure $ IfStatement
                        (foldr1 (LogicalExpression LogicalAndOperator) conds)
                        body Nothing
        pure $ CallExpression (Left' $ ArrowFunctionExpression [] $
            Left' $ Block $ aliStmts ++ N.toList altStmts) []
    toJS (LamExpr (Lam ident expr)) =
        ArrowFunctionExpression . pure . IdentifierPattern
            <$> toJS ident <*> (Right' <$> toJS expr)
    toJS (NativeExpr native) = toJS native

instance ToJS Native Expression where
    toJS (Native (P expr)) = pure expr

instance ToJS IdentBind Identifier where
    toJS (IdentBind ident) = toJS ident

instance ToJS IdentRef Identifier where
    toJS (IdentRef _ ident) = toJS ident

-- | Convert dtfpl 'Ident' to JS 'Identifier', trying to preserve as much of the
-- original name as possible.
instance ToJS Ident Identifier where
    toJS = mkIdentifierE . convertIdent
      where convertIdent (Ident s)
                | s `elem` reservedWords = "$r" ++ s ++ "$"
                | otherwise = head s : concatMap convertChar (tail s)
            convertIdent (GenIdentPart (A ident _) (P n)) =
                "$g" ++ convertIdent ident ++ "$p" ++ show n ++ "$"
            convertIdent (GenIdentFull (P n)) = "$f" ++ show n ++ "$"
            convertChar '-' = "_"
            convertChar '_' = "$u$"
            convertChar '$' = "$d$"
            convertChar c
                | isValidIdentifierPart c = [c]
                | otherwise = "$c" ++ show (fromEnum c) ++ "$"

instance ToJS Lit Literal where
    toJS (NumLit n) = pure $ NumberLiteral n
    toJS (StrLit s) = pure $ StringLiteral s
