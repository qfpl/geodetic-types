{-# LANGUAGE NoImplicitPrelude #-}

module Geodetics.Types.LatLon(
  LatLon(..)
, HasLatLon(..)
, AsLatLon(..)
, ManyLatLon(..)
, GetLatLon(..)
, SetLatLon(..)
, FoldLatLon(..)
, IsLatLon(..)  
) where
 
import Control.Category(id)
import Control.Lens(Lens', Prism', Traversal', Getter, Setter', Fold, Iso')
import Data.Eq(Eq)
import Data.Functor(fmap)
import Data.Ord(Ord)
import Geodetics.Types.Latitude(Latitude, HasLatitude(latitude), SetLatitude(_SetLatitude), GetLatitude(_GetLatitude), FoldLatitude(_FoldLatitude), ManyLatitude(_ManyLatitude))
import Geodetics.Types.Longitude(Longitude, HasLongitude(longitude), SetLongitude(_SetLongitude), GetLongitude(_GetLongitude), FoldLongitude(_FoldLongitude), ManyLongitude(_ManyLongitude))
import Prelude(Show)

data LatLon =
  LatLon
    Latitude
    Longitude
  deriving (Eq, Ord, Show)

instance HasLatitude LatLon where
  latitude f (LatLon lt ln) =
    fmap (\lt' -> LatLon lt' ln) (f lt)

instance SetLatitude LatLon where
  _SetLatitude =
    latitude

instance GetLatitude LatLon where
  _GetLatitude =
    latitude

instance FoldLatitude LatLon where
  _FoldLatitude =
    latitude

instance ManyLatitude LatLon where
  _ManyLatitude =
    latitude

instance HasLongitude LatLon where
  longitude f (LatLon lt ln) =
    fmap (\ln' -> LatLon lt ln') (f ln)

instance SetLongitude LatLon where
  _SetLongitude =
    longitude

instance GetLongitude LatLon where
  _GetLongitude =
    longitude

instance FoldLongitude LatLon where
  _FoldLongitude =
    longitude

instance ManyLongitude LatLon where
  _ManyLongitude =
    longitude

class HasLatLon a where
  latlon ::
    Lens' a LatLon

instance HasLatLon LatLon where
  latlon =
    id

class ManyLatLon a => AsLatLon a where
  _LatLon ::
    Prism' a LatLon

instance AsLatLon LatLon where
  _LatLon =
    id

class (FoldLatLon a, SetLatLon a) => ManyLatLon a where
  _ManyLatLon ::
    Traversal' a LatLon

instance ManyLatLon LatLon where
  _ManyLatLon =
    id

class FoldLatLon a => GetLatLon a where
  _GetLatLon ::
    Getter a LatLon
    
instance GetLatLon LatLon where
  _GetLatLon =
    id

class SetLatLon a where
  _SetLatLon ::
    Setter' a LatLon
    
instance SetLatLon LatLon where
  _SetLatLon =
    id

class FoldLatLon a where
  _FoldLatLon ::
    Fold a LatLon
    
instance FoldLatLon LatLon where
  _FoldLatLon =
    id

class (HasLatLon a, AsLatLon a) => IsLatLon a where
  _IsLatLon ::
    Iso' a LatLon
    
instance IsLatLon LatLon where
  _IsLatLon =
    id
