{-# LANGUAGE RecordWildCards #-}

module Language.Dtfpl.Parser where

import           Control.Category           ((>>>))
import           Data.Char
import           Data.Functor
import           Data.List
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer (IndentOpt (..), indentBlock,
                                             indentGuard, indentLevel,
                                             nonIndented)

import           Language.Dtfpl.Syntax

type Parser = Parsec PError String

type LocParser n = Parser (A n Loc)

type LocParser' n = Parser (A' n Loc)

data Loc = Loc { start :: SourcePos, end :: SourcePos }

instance Show Loc where
    show Loc {..} = showSourcePos start ++ "-" ++ showSourcePos end
      where showSourcePos SourcePos {..} =
                show (unPos sourceLine) ++ ":" ++ show (unPos sourceColumn)

data PError
    = ReservedWordIdentError String
    deriving (Eq, Ord, Show)

instance ShowErrorComponent PError where
    showErrorComponent (ReservedWordIdentError reservedWord) =
        reservedWord ++ " is a reserved word"

prog :: LocParser' Prog
prog = addLoc (Prog <$> many (nonIndented scn decl <* scn)) <* eof

decl :: LocParser' Decl
decl = def <|> let_
  where def = indentBlock scn $ do
            s <- getPosition
            lexeme sdef
            name <- ident
            let result alts = pure $ A (Def name alts) $
                    Loc s $ end $ ann $ last alts
            pure $ IndentSome Nothing result defAlt
        let_ = addLoc $ exprBlock $
            lexeme slet *> (Let <$> lexeme ident <* equals)

defAlt :: LocParser' DefAlt
defAlt = addLoc $ exprBlock $ DefAlt <$> (some (lexeme pat) <* arrow)

pat :: LocParser' Pat
pat = addLoc $ VarPat <$> ident

exprBlock :: Parser (A' Expr Loc -> a) -> Parser a
exprBlock p = do
    i <- indentLevel
    p <*> (isc i *> expr i <* lookAhead (scn1 <|> hidden eof))

expr :: Pos -> LocParser' Expr
expr i = app <|> notApp
  where notApp = var <|> if_ <|> lit <|> par
        var = addLoc $ Var <$> try ident
        lit = addLoc $ Lit <$> literal
        par = parens (sc' *> expr i)
        if_ = addLoc $ If
            <$> (sif *> sc1' *> expr i)
            <*> (sthen *> sc1' *> expr i)
            <*> (selse *> sc1' *> expr i)
        app = foldl1' combine <$> notApp `sepEndBy1` try sc1'
          where combine f x = A (App f x) $ Loc (start (ann f)) (end (ann x))
        sc' = isc i
        sc1' = isc1 i

reservedWords :: [String]
reservedWords = ["def", "let", "if", "then", "else"]

sdef, slet, sif, sthen, selse :: Parser ()
[sdef, slet, sif, sthen, selse] = map
    (string >>> (*> notFollowedBy (satisfy isIdentTailChar)))
    reservedWords

arrow :: Parser ()
arrow = void $ string "->"

equals :: Parser ()
equals = void $ string "="

{-# ANN reservedChars "HLint: ignore Use String" #-}
reservedChars :: [Char]
reservedChars = "()[]{}.,:;\\\""

parens :: Parser a -> Parser a
parens = between (string "(") (string ")")

ident :: LocParser Ident
ident = addLoc $ Ident <$> do
    identifier <- (:) <$> letterChar <*> takeWhileP Nothing isIdentTailChar
    if identifier `elem` reservedWords
        then customFailure $ ReservedWordIdentError identifier
        else return identifier

isIdentTailChar :: Char -> Bool
isIdentTailChar x = isPrint x
                 && not (isSeparator x)
                 && x `notElem` reservedChars

literal :: LocParser Literal
literal = numLit <|> strLit
  where numLit = addLoc $ NumLit . read <$>
            (option id ((:) <$> char '-') <*>
                ((++) <$> digits <*>
                    option "" ((:) <$> char '.' <*> digits)))
          where digits = takeWhile1P (Just "digit") isDigit
        strLit = addLoc $ StrLit <$>
            (quote *> manyTill (escape <|> notChar '\n') quote)
          where quote = char '"'
                escape = char '\\' *> choice
                    [ char '\\'
                    , char '"'
                    , char 'n' $> '\n' ]

-- autoLoc :: forall p n m. (Data (n Loc), Data m) => p m -> n Loc -> A' n Loc
-- autoLoc _ n = A n $ Loc (start (ann (head xs))) (end (ann (last xs)))
--   where xs :: [A m Loc]
--         xs = childrenBi n

addLoc :: Parser n -> LocParser n
addLoc p = do
    s <- getPosition
    A <$> p <*> (Loc s <$> getPosition)

sc :: Parser ()
sc = hidden $ void $ takeWhile1P Nothing (== ' ')

scn :: Parser ()
scn = hidden space

scn1 :: Parser ()
scn1 = hidden space1

isc :: Pos -> Parser ()
isc = void . indentGuard scn GT

isc1 :: Pos -> Parser ()
isc1 = void . indentGuard scn1 GT

lexeme :: Parser a -> Parser a
lexeme = (<* sc)
