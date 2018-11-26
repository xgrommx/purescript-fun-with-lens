module Data.Prism.Variant where

import Data.Lens (Prism', prism')
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant, inj, prj)
import Prim.Row as Row

_Variant 
  :: forall l t v a
   . IsSymbol l 
  => Row.Cons l a t v 
  => SProxy l 
  -> Prism' (Variant v) a
_Variant l = prism' (inj l) (prj l)