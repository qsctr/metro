module Language.Dtfpl.Err.Format
    ( formatErr
    ) where

import           Language.Dtfpl.Err
import           Language.Dtfpl.Err.ErrLoc
import           Language.Dtfpl.Err.ErrMessage
import           Language.Dtfpl.Parse.Loc
import           Text.Megaparsec.Error

formatErr :: Err -> String
formatErr err = errType err ++ "\n" ++ case err of
    InternalErr e -> formatErrMessage e
    ParseErr e    -> errorBundlePretty e
    SimplifyErr e -> formatErrMessageLoc e

errType :: Err -> String
errType (InternalErr _) = "Internal error"
errType (ParseErr _)    = "Parse error"
errType (SimplifyErr _) = "Simplify error"

formatErrMessageLoc :: (ErrMessage a, ErrLoc a) => a -> String
formatErrMessageLoc err = case errLoc err of
    Just loc -> formatLoc loc ++ "\n" ++ formatErrMessage err
    Nothing  -> formatErrMessage err

formatErrMessage :: ErrMessage a => a -> String
formatErrMessage = unlines . errMessage
