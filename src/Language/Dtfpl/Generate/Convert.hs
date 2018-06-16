{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Language.Dtfpl.Generate.Convert
    ( convert
    ) where

import           Control.Monad
import qualified Data.List.NonEmpty                as N
import           Data.Traversable

import           Language.Dtfpl.M
import           Language.Dtfpl.Syntax
import           Language.ECMAScript.Syntax
import           Language.ECMAScript.Syntax.Util
import           Language.ECMAScript.Syntax.Verify
import           Util

convert :: A Prog Core -> M Program
convert = toJS

class ToJS n js where
    toJS :: n Core -> M js

instance ToJS n js => ToJS (A n) js where
    toJS (A n _) = toJS n

instance ToJS Prog Program where
    toJS (Prog (T decls)) = Program ScriptSourceType
        <$> traverse (fmap (Left' . DeclarationStatement) . toJS) decls

instance ToJS Decl Declaration where
    toJS (Let ident expr) = constDecl <$> toJS ident <*> toJS expr

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
                    Just ident ->
                        ((ident, expr) : alis, genLoc $ VarExpr $ genLoc ident))
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
            binds <- forMaybe checkPairs $ \(headExpr, A pat _) -> case pat of
                    VarPat ident -> Just $
                        DeclarationStatement . flip constDecl headExpr
                            <$> toJS ident
                    WildPat -> Nothing
            ret <- ReturnStatement . Just <$> toJS altExpr
            let body = BlockStatement $ Block $ binds ++ [ret]
            case nonCheckPairs of
                [] -> pure body
                _ -> do
                    conds <- for nonCheckPairs $ \(headExpr, A pat _) ->
                        case pat of
                            LitPat lit ->
                                BinaryExpression StrictEqualOperator headExpr
                                . LiteralExpression <$> toJS lit
                    pure $ IfStatement
                        (foldr1 (LogicalExpression LogicalAndOperator) conds)
                        body Nothing
        pure $ CallExpression (Left' $ ArrowFunctionExpression [] $
            Left' $ Block $ aliStmts ++ N.toList altStmts) []
    toJS (LamExpr (Lam ident expr)) =
        ArrowFunctionExpression . pure . IdentifierPattern
            <$> toJS ident <*> (Right' <$> toJS expr)

instance ToJS Ident Identifier where
    toJS = mkIdentifier . convertIdent
      where convertIdent (Ident s)
                | s `elem` reservedWords = '$' : s
                | otherwise = head s : concatMap convertChar (tail s)
            convertIdent (GenIdentPart (A ident _) (P n)) =
                '_' : convertIdent ident ++ "$_" ++ show n
            convertIdent (GenIdentFull (P n)) = '_' : show n
            convertChar '-' = "_"
            convertChar '_' = "__"
            convertChar '$' = "$$"
            convertChar c
                | isValidIdentifierPart c = [c]
                | otherwise = '$' : show (fromEnum c) ++ "_"

instance ToJS Lit Literal where
    toJS (NumLit n) = pure $ NumberLiteral n
    toJS (StrLit s) = pure $ StringLiteral s
