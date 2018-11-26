module Main where

import Prelude

import Data.Array (head, (!!))
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Prism', filtered, is, matching, over, re, traversed, (.~), (^.), (^..), (^?), (%~))
import Data.Lens.Extra (below)
import Data.Lens.Index (ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Prism.Variant (_Variant)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Console (logShow)
import Heterogeneous.Mapping as H

newtype Circle = Circle { circleRadius :: Number, circleCenter :: Tuple Number Number }

derive instance newtypeCircle :: Newtype Circle _
derive instance genericCircle :: Generic Circle _

instance showCircle :: Show Circle where
  show c = genericShow c

instance eqCircle :: Eq Circle where
  eq x y = genericEq x y

newtype Square = Square { squareTopLeft :: Tuple Number Number, squareWidth :: Number }

derive instance newtypeSquare :: Newtype Square _
derive instance genericSquare :: Generic Square _

instance showSquare :: Show Square where
  show c = genericShow c

instance eqSquare :: Eq Square where
  eq x y = genericEq x y

class Resizable a where
  resize :: Number -> a -> a

instance resizableCircle :: Resizable Circle where
  resize f (Circle r) = Circle $ r { circleRadius = f * r.circleRadius }

instance resizableSquare :: Resizable Square where
  resize f (Square r) = Square $ r { squareWidth = f * r.squareWidth }

type Shape r = (circle :: Circle, square :: Square | r)

_circle = (SProxy :: SProxy "circle")
_square = (SProxy :: SProxy "square")
_circleCenter = (SProxy :: SProxy "circleCenter")

-- prisms

_Circle :: forall r. Prism' (Variant (circle :: Circle | r)) Circle
_Circle = _Variant _circle

_Square :: forall r. Prism' (Variant (square :: Square | r)) Square
_Square = _Variant _square

-- smart constructors

circle :: forall r. Number -> Tuple Number Number -> Variant (circle :: Circle | r)
circle r c = { circleRadius: r, circleCenter: c } ^. re (_Circle <<< _Newtype)

square :: forall r. Tuple Number Number -> Number -> Variant (square :: Square | r)
square tl w = { squareTopLeft: tl, squareWidth: w } ^. re (_Square <<< _Newtype)

shapes :: forall r. Array (Variant (Shape r))
shapes = [ square (Tuple 1.0 2.0) 5.0, circle 4.0 (Tuple 2.0 3.0), circle 1.0 (Tuple 5.0 7.0) ]

shapes' :: forall r. Array (Variant (Shape r))
shapes' = [ circle 4.0 (Tuple 2.0 3.0), circle 1.0 (Tuple 5.0 7.0) ]

data ResizeCase n = ResizeCase n

instance resizeCase :: Resizable r => H.Mapping (ResizeCase Number) r r where
  mapping (ResizeCase n) r = resize n r

resize' :: forall t. H.HMap (ResizeCase Number) t t  => Number -> t -> t
resize' n = H.hmap (ResizeCase n)

main :: Effect Unit
main = do
  logShow $ head shapes <#> (_ ^? _Circle)
  logShow $ shapes !! 1 <#> (_ ^? _Circle)
  logShow $ (head shapes <#> (is _Circle)) :: Maybe Boolean
  logShow $ (shapes !! 1 <#> (is _Circle)) :: Maybe Boolean
  logShow $ head shapes <#> (_ ^? _Circle <<< _Newtype <<< (prop _circleCenter))
  logShow $ shapes !! 1 <#> (_ ^? _Circle <<< _Newtype <<< (prop _circleCenter))
  logShow $ (head shapes <#> (_ # _Circle <<< _Newtype <<< (prop _circleCenter) .~ (Tuple 1000.0 2000.0))) :: Maybe (Variant (Shape ()))
  logShow $ (shapes !! 1 <#> (_ # _Circle <<< _Newtype <<< (prop _circleCenter) .~ (Tuple 1000.0 2000.0))) :: Maybe (Variant (Shape ()))
  logShow $ (over (traversed <<< _Circle) (resize 1000.0) shapes) :: Array (Variant (Shape ()))
  logShow $ (shapes ^.. (traversed <<< filtered (is _Square))) :: List (Variant (Shape ()))
  logShow $ (shapes ^? ix 1) :: (Maybe (Variant (Shape ())))
  logShow $ (shapes # (ix 2 <<< _Circle <<< _Newtype <<< (prop _circleCenter)) .~ (Tuple 1000.0 2000.0)) :: Array (Variant (Shape ()))
  logShow $ (matching (below _Circle) shapes) :: Either (Array (Variant (Shape ()))) (Array Circle)
  logShow $ (matching (below _Circle) shapes') :: Either (Array (Variant (Shape ()))) (Array Circle)
  logShow $ (head shapes <#> (_ # matching _Circle)) :: Maybe (Either (Variant (Shape ())) Circle)
  logShow $ (head shapes' <#> (_ # matching _Circle)) :: Maybe (Either (Variant (Shape ())) Circle)
  logShow $ shapes' ^.. below _Circle
  logShow $ shapes ^.. below _Square
  logShow $ ([shapes, shapes'] # (traversed <<< traversed) %~ (resize' 10.0)) :: Array (Array (Variant (Shape ())))
