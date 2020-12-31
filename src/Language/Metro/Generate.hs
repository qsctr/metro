{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Generate JS from Metro Core
module Language.Metro.Generate
    ( generate
    ) where

import           Data.List
import qualified Data.List.NonEmpty                 as N
import           Data.Traversable
import           Polysemy
import           Polysemy.Error
import           Polysemy.Reader
import qualified System.Path                        as P

import           Language.Metro.Config
import           Language.Metro.Err
import           Language.Metro.Generate.Identifier
import           Language.Metro.Module.Context
import           Language.Metro.Module.Extensions
import           Language.Metro.Module.Main
import           Language.Metro.Syntax
import           Language.Metro.Syntax.Util
import           Language.Metro.Util
import           Language.Metro.Util.CEPath
import           Language.Metro.Util.EPath
import           Language.Metro.Util.FS
import           Language.ECMAScript.Syntax
import           Language.ECMAScript.Syntax.Util
import           Language.ECMAScript.Syntax.Verify

type GenerateEffs = '[Reader ModuleContext, Reader Config, Error Err, FS]

generate :: Members GenerateEffs r => A Mod Core -> Sem r Program
generate = toJS

-- | Typeclass for metro core node @n@ which can be converted to JS node @js@.
class ToJS n js where
    -- | Convert a metro core node to a js node.
    toJS :: Members GenerateEffs r => n Core -> Sem r js

class PToJS t js where
    pToJS :: Members GenerateEffs r => t -> Sem r js

instance PToJS t js => ToJS (P t) js where
    toJS (P x) = pToJS x

instance (ToJS n js, Traversable t) => ToJS (T t n) (t js) where
    toJS (T xs) = traverse toJS xs

-- | Currently we just throw away the annotations.
-- Might add source maps later on.
instance ToJS n js => ToJS (A n) js where
    toJS (A n _) = toJS n

instance ToJS Mod Program where
    toJS (Mod imps tls) = do
        jImps <- toJS imps
        jTls <- toJS tls
        let stmts = map Right' jImps ++ jTls
        isMain <- isMainModule
        Program ModuleSourceType <$>
            if isMain && hasMainFn (unT tls)
                then do
                    iMain <- toJS mainFn
                    iProcess <- mkIdentifierE "process"
                    iArgv <- mkIdentifierE "argv"
                    iSlice <- mkIdentifierE "slice"
                    let mainCall = Left' $ ExpressionStatement $ CallExpression
                            (Left' $ IdentifierExpression iMain)
                            [Left' $ CallExpression
                                (Left' $ MemberExpression $ Member
                                    (Left' $ MemberExpression $ Member
                                        (Left' $ IdentifierExpression iProcess)
                                        (Right' iArgv))
                                    (Right' iSlice))
                                [Left' $ LiteralExpression $ NumberLiteral 2]]
                    pure $ stmts ++ [mainCall]
                else pure stmts

instance ToJS Import ModuleDeclaration where
    toJS (Import (P path) modName) = do
        jModName <- toJS modName
        pure $ case replaceExt outExt (ePath path) of
            EPath outPath ->
                let spec = P.withAbsRel
                        (\absPath -> "file://" ++ P.toString absPath)
                        (\relPath -> P.toString P.currentDir
                            ++ [P.pathSeparator] ++ P.toString relPath)
                        outPath
                in  ImportDeclaration
                        [Right' $ Right' $ ImportNamespaceSpecifier jModName]
                        $ StringLiteral spec

instance PToJS ModName Identifier where
    pToJS (ModName atoms) = mkIdentifierE $ "$m_"
        ++ intercalate "$_" (map (identifierize . unModAtom) $ N.toList atoms)

instance ToJS TopLevel (Either' Statement ModuleDeclaration) where
    toJS (TLDecl Exp decl) =
        Right' . ExportNamedDeclarationDeclaration <$> toJS decl
    toJS (TLDecl Priv decl) = Left' . DeclarationStatement <$> toJS decl

instance ToJS Decl Declaration where
    toJS (Let ident expr) = constDecl <$> toJS ident <*> toJS expr
    toJS (Def defHead _)  = absurdP defHead

-- | For the 'Case' expression:
-- Converts each 'Case' into an IIFE where inside the function body there are
-- if statements corresponding to each pattern match.
instance ToJS Expr Expression where
    toJS (VarExpr identRef) = toJS identRef
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
    toJS (Native (A (P expr) _)) = pure expr

instance ToJS IdentBind Identifier where
    toJS (IdentBind ident) = toJS ident

instance ToJS IdentRef Expression where
    toJS (IdentRef (T refBind) _) = case refBind of
        Left (ImpIdentBind modName identBind) -> do
            jModName <- pToJS modName
            jIdent <- toJS identBind
            pure $ MemberExpression $
                Member (Left' $ IdentifierExpression jModName) $ Right' jIdent
        Right identBind -> IdentifierExpression <$> toJS identBind

-- | Convert metro 'Ident' to JS 'Identifier', trying to preserve as much of the
-- original name as possible.
instance ToJS Ident Identifier where
    toJS = mkIdentifierE . convertIdent
      where convertIdent (Ident s)
                | s `elem` reservedWords = "$r" ++ s ++ "$"
                | otherwise = identifierize s
            convertIdent (GenIdentPart (A ident _) (P n)) =
                "$g" ++ convertIdent ident ++ "$p" ++ show n ++ "$"
            convertIdent (GenIdentFull (P n)) = "$f" ++ show n ++ "$"

instance ToJS Lit Literal where
    toJS (NumLit n) = pure $ NumberLiteral n
    toJS (StrLit s) = pure $ StringLiteral s
