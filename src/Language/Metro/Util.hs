-- | General-purpose utility functions
module Language.Metro.Util
    ( (.:)
    , traverseMaybe
    , forMaybe
    , whenM
    ) where

import           Control.Monad
import           Data.Maybe

-- | Compose a one-argument function with a two-argument function,
-- yielding a two-argument function.
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .: g = \x -> f . g x
infixr 9 .:

-- | Applicative version of 'mapMaybe'.
traverseMaybe :: Applicative f => (a -> Maybe (f b)) -> [a] -> f [b]
traverseMaybe = sequenceA .: mapMaybe

-- | Flipped version of 'traverseMaybe'.
forMaybe :: Applicative f => [a] -> (a -> Maybe (f b)) -> f [b]
forMaybe = flip traverseMaybe

-- | Monadic version of 'when'.
whenM :: Monad m => m Bool -> m () -> m ()
whenM cm x = do
    c <- cm
    when c x
