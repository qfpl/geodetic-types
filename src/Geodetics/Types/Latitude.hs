{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Geodetics.Types.Latitude(
  Latitude(..)
, HasLatitude(..)
, AsLatitude(..)
, ManyLatitude(..)
, GetLatitude(..)
, SetLatitude(..)
, FoldLatitude(..)
, IsLatitude(..)  
) where
  
import Control.Category((.), id)
import Control.Lens(Lens', Prism', Traversal', Getter, Setter', Fold, Iso', Wrapped(_Wrapped'), Unwrapped, iso, prism)
import Data.Either(Either(Right))
import Data.Eq(Eq)
import Data.Functor(fmap)
import Data.Ord(Ord)
import Numeric.Units.Dimensional.Prelude(Angle)
import Prelude(Double, Show)

newtype Latitude =
  Latitude
    (Angle Double)
  deriving (Eq, Ord, Show)

instance Wrapped Latitude where
  type Unwrapped Latitude =
    Angle Double
  _Wrapped' =
    iso
      (\(Latitude x) -> x)
      Latitude

class HasLatitude a where
  latitude ::
    Lens' a Latitude
  latitudeAngle ::
    Lens' a (Angle Double)
  {-# INLINE latitudeAngle #-}
  latitudeAngle =
    latitude . latitudeAngle

instance HasLatitude Latitude where
  latitude =
    id
  latitudeAngle k (Latitude x) =
    fmap Latitude (k x)

class ManyLatitude a => AsLatitude a where
  _Latitude ::
    Prism' a Latitude
  _LatitudeFields ::
    Prism' a (Angle Double)
  _LatitudeFields =
    _Latitude . _LatitudeFields

instance AsLatitude Latitude where
  _Latitude =
    id
  _LatitudeFields =
    prism
      Latitude
      (\(Latitude x) -> Right x)

class (FoldLatitude a, SetLatitude a) => ManyLatitude a where
  _ManyLatitude ::
    Traversal' a Latitude

instance ManyLatitude Latitude where
  _ManyLatitude =
    id

class FoldLatitude a => GetLatitude a where
  _GetLatitude ::
    Getter a Latitude
    
instance GetLatitude Latitude where
  _GetLatitude =
    id

class SetLatitude a where
  _SetLatitude ::
    Setter' a Latitude
    
instance SetLatitude Latitude where
  _SetLatitude =
    id

class FoldLatitude a where
  _FoldLatitude ::
    Fold a Latitude
    
instance FoldLatitude Latitude where
  _FoldLatitude =
    id

class (HasLatitude a, AsLatitude a) => IsLatitude a where
  _IsLatitude ::
    Iso' a Latitude
    
instance IsLatitude Latitude where
  _IsLatitude =
    id
