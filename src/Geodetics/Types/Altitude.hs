{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module Geodetics.Types.Altitude(
  Altitude(..)
, HasAltitude(..)
, AsAltitude(..)
, ManyAltitude(..)
, GetAltitude(..)
, SetAltitude(..)
, FoldAltitude(..)
, IsAltitude(..)
, groundPosition
) where

import Control.Category((.), id)
import Control.Lens(Lens', Prism', Traversal', Getter, Setter', Fold, Iso', Wrapped(_Wrapped'), Unwrapped, iso, prism, set)
import Data.Either(Either(Right))
import Data.Eq(Eq)
import Data.Functor(fmap)
import Data.Ord(Ord)
import Numeric.Units.Dimensional.Prelude(Length, _0)
import Prelude(Double, Show)

newtype Altitude =
  Altitude
    (Length Double)
  deriving (Eq, Ord, Show)
  
instance Wrapped Altitude where
  type Unwrapped Altitude =
    Length Double
  _Wrapped' =
    iso
      (\(Altitude x) -> x)
      Altitude

class HasAltitude a where
  altitude ::
    Lens' a Altitude
  altitudeLength ::
    Lens' a (Length Double)
  {-# INLINE altitudeLength #-}
  altitudeLength =
    altitude . altitudeLength

instance HasAltitude Altitude where
  altitude =
    id
  altitudeLength k (Altitude x) =
    fmap Altitude (k x)

class ManyAltitude a => AsAltitude a where
  _Altitude ::
    Prism' a Altitude
  _AltitudeFields ::
    Prism' a (Length Double)
  _AltitudeFields =
    _Altitude . _AltitudeFields

instance AsAltitude Altitude where
  _Altitude =
    id
  _AltitudeFields =
    prism
      Altitude
      (\(Altitude x) -> Right x)

class (FoldAltitude a, SetAltitude a) => ManyAltitude a where
  _ManyAltitude ::
    Traversal' a Altitude

instance ManyAltitude Altitude where
  _ManyAltitude =
    id

class FoldAltitude a => GetAltitude a where
  _GetAltitude ::
    Getter a Altitude
    
instance GetAltitude Altitude where
  _GetAltitude =
    id

class SetAltitude a where
  _SetAltitude ::
    Setter' a Altitude
    
instance SetAltitude Altitude where
  _SetAltitude =
    id

class FoldAltitude a where
  _FoldAltitude ::
    Fold a Altitude
    
instance FoldAltitude Altitude where
  _FoldAltitude =
    id

class (HasAltitude a, AsAltitude a) => IsAltitude a where
  _IsAltitude ::
    Iso' a Altitude
    
instance IsAltitude Altitude where
  _IsAltitude =
    id
groundPosition ::
  HasAltitude a =>
  a
  -> a
groundPosition =
  set altitude (Altitude _0)
