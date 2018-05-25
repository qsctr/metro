{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.Dtfpl.Generate.Convert
    ( -- convert
    ) where

import           Language.Dtfpl.M
-- import           Language.Dtfpl.Syntax
import           Language.ECMAScript.Syntax.Verify
import           Language.ECMAScript.Syntax

-- convert :: AProg a -> M Program
-- convert = toJS

-- class ToJS n js where
--     toJS :: n -> M js

-- instance ToJS n js => ToJS (A n a) js where
--     toJS (A n _) = toJS n

-- -- instance ToJS (Prog a) Program where
-- --     toJS (Prog decls) = Program $ map toJS decls

-- -- instance ToJS (Decl a) Statement where
-- --     toJS (Def i alts) = VariableDeclarationStatement $
-- --         VariableDeclaration VarVariableDeclaration
-- --             [VariableDeclarator
-- --                 <$> (IdentifierPattern <$> toJS i)
-- --                 <*> (Just <$> )]

-- instance ToJS (Expr a) Expression where
--     toJS (VarExpr i) = IdentifierExpression <$> toJS i
--     toJS (LitExpr l) = LiteralExpression <$> toJS l
--     toJS (App f x)   = CallExpression <$> toJS f <*> (pure <$> toJS x)
--     toJS (If c t f)  = ConditionalExpression <$> toJS c <*> toJS t <*> toJS f

-- instance ToJS Ident Identifier where
--     toJS (Ident s@(h:t))
--         | s `elem` reservedWords = mkIdentifier $ '$' : s
--         | otherwise = mkIdentifier $ h : concatMap convertChar t
--       where convertChar '-' = "_"
--             convertChar '_' = "__"
--             convertChar '$' = "$$"
--             convertChar c
--                 | isValidIdentifierPart c = [c]
--                 | otherwise = '$' : show (fromEnum c)

-- instance ToJS Lit Literal where
--     toJS (NumLit n) = pure $ NumberLiteral n
--     toJS (StrLit s) = pure $ StringLiteral s
