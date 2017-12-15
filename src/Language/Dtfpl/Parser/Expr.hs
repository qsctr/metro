module Language.Dtfpl.Parser.Expr where

import           Data.List
import           Language.Dtfpl.Parser.Ident
import           Language.Dtfpl.Parser.Lexer
import           Language.Dtfpl.Parser.Literal
import           Language.Dtfpl.Syntax
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

expr :: Pos -> Parser (Expr ())
expr indent = expr'
  where expr' = app <|> term
        term =  Var () <$> ident
            <|> Lit () <$> literal
            <|> parens (scn' *> expr')
        app = foldl1' (App ()) <$> term `sepEndBy1` try space1'
        scn' = L.indentGuard scn GT indent
        space1' = L.indentGuard space1 GT indent
