-- | The 'ErrMessage' class and 'errMessage' method.
module Language.Metro.Err.ErrMessage
    ( ErrMessage (..)
    ) where

-- | Class for errors that have an error message.
class ErrMessage a where
    -- | Describe the error in a user-friendly way.
    -- Each string in the list will be shown as a separate line.
    errMessage :: a -> [String]
