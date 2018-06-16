module Language.Dtfpl.Err.ShowErr
    ( ShowErr (..)
    ) where

import Text.Megaparsec.Error

class ShowErr a where
    showErr :: a -> [String]

instance (Ord t, ShowToken t, ShowErrorComponent e) =>
         ShowErr (ParseError t e) where
    showErr parseError = [parseErrorPretty parseError]
