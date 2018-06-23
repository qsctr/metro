-- | The 'ShowErr' class and 'showErr' method.
module Language.Dtfpl.Err.ShowErr
    ( ShowErr (..)
    ) where

import Text.Megaparsec.Error

-- | Class for errors that can be shown.
class ShowErr a where
    -- | Show the error in a user-friendly way.
    -- Each string in the list will be shown as a separate line.
    -- Instances should list the lines from least specific to most specific.
    showErr :: a -> [String]

-- | Instance for megaparsec's 'ParseError' error type.
-- In this module to prevent orphan instance.
instance (Ord t, ShowToken t, ShowErrorComponent e) =>
         ShowErr (ParseError t e) where
    showErr parseError = [parseErrorPretty parseError]
