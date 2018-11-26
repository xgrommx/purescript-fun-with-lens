module Data.Lens.Extra where

import Prelude

import Data.Either (Either(..))
import Data.Lens (APrism', Prism', prism, withPrism)
import Data.Traversable (class Traversable, traverse)

below :: forall f s a. Traversable f => APrism' s a -> Prism' (f s) (f a)
below k =
  withPrism k $ \bt seta ->
    prism (map bt) $ \s ->
      case traverse seta s of
        Left _  -> Left s
        Right t -> Right t