{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Geodetics.Types.Longitude(
  Longitude(..)
, HasLongitude(..)
, AsLongitude(..)
, ManyLongitude(..)
, GetLongitude(..)
, SetLongitude(..)
, FoldLongitude(..)
, IsLongitude(..)
) where

import Control.Category((.), id)
import Control.Lens(Lens', Prism', Traversal', Getter, Setter', Fold, Iso', Wrapped(_Wrapped'), Unwrapped, iso, prism)
import Data.Either(Either(Right))
import Data.Eq(Eq)
import Data.Functor(fmap)
import Data.Ord(Ord)
import Numeric.Units.Dimensional.Prelude(Angle)
import Prelude(Double, Show)

newtype Longitude =
  Longitude
    (Angle Double)
  deriving (Eq, Ord, Show)
  
instance Wrapped Longitude where
  type Unwrapped Longitude =
    Angle Double
  _Wrapped' =
    iso
      (\(Longitude x) -> x)
      Longitude

class HasLongitude a where
  longitude ::
    Lens' a Longitude
  longitudeAngle ::
    Lens' a (Angle Double)
  {-# INLINE longitudeAngle #-}
  longitudeAngle =
    longitude . longitudeAngle

instance HasLongitude Longitude where
  longitude =
    id
  longitudeAngle k (Longitude x) =
    fmap Longitude (k x)

class ManyLongitude a => AsLongitude a where
  _Longitude ::
    Prism' a Longitude
  _LongitudeFields ::
    Prism' a (Angle Double)
  _LongitudeFields =
    _Longitude . _LongitudeFields

instance AsLongitude Longitude where
  _Longitude =
    id
  _LongitudeFields =
    prism
      Longitude
      (\(Longitude x) -> Right x)

class (FoldLongitude a, SetLongitude a) => ManyLongitude a where
  _ManyLongitude ::
    Traversal' a Longitude

instance ManyLongitude Longitude where
  _ManyLongitude =
    id

class FoldLongitude a => GetLongitude a where
  _GetLongitude ::
    Getter a Longitude
    
instance GetLongitude Longitude where
  _GetLongitude =
    id

class SetLongitude a where
  _SetLongitude ::
    Setter' a Longitude
    
instance SetLongitude Longitude where
  _SetLongitude =
    id

class FoldLongitude a where
  _FoldLongitude ::
    Fold a Longitude
    
instance FoldLongitude Longitude where
  _FoldLongitude =
    id

class (HasLongitude a, AsLongitude a) => IsLongitude a where
  _IsLongitude ::
    Iso' a Longitude
    
instance IsLongitude Longitude where
  _IsLongitude =
    id
