module Language.Dtfpl.Util
    ( (.:)
    , traverseMaybe
    , forMaybe
    ) where

import Data.Maybe

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
f .: g = \x -> f . g x
infixr 9 .:

traverseMaybe :: Applicative f => (a -> Maybe (f b)) -> [a] -> f [b]
traverseMaybe = sequenceA .: mapMaybe

forMaybe :: Applicative f => [a] -> (a -> Maybe (f b)) -> f [b]
forMaybe = flip traverseMaybe
