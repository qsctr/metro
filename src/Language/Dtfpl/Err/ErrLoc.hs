-- | The 'ErrLoc' class and 'errLoc' method.
module Language.Dtfpl.Err.ErrLoc
    ( ErrLoc (..)
    ) where

import           Language.Dtfpl.Parser.Loc

-- | Class for errors that may have a location.
class ErrLoc a where
    -- | Get the location of an error, if present.
    errLoc :: a -> Maybe Loc
