{-# LANGUAGE NoImplicitPrelude #-}

module Geodetics.Types.Ellipsoid(
  Ellipsoid(..)
, HasEllipsoid(..)
, AsEllipsoid(..)
, ManyEllipsoid(..)
, GetEllipsoid(..)
, SetEllipsoid(..)
, FoldEllipsoid(..)
, IsEllipsoid(..)
, _GRS80
, _GRS67
, _Ans
, _WGS72
, _AU1965
, _Krasovsky1940
, _International1924
, _Hayford1909
, _Airy1830
, _Everest1830
, _Clarke1858
, _Clarke1880
) where

import Control.Category((.), id)
import Control.Lens(Lens', Prism', Traversal', Getter, Setter', Fold, Iso', prism)
import Data.Either(Either(Right))
import Data.Eq(Eq)
import Data.Ord(Ord)
import Data.Functor(fmap)
import Data.Tuple(uncurry)
import Numeric.Units.Dimensional.Prelude(Length, Dimensionless, (*~), meter, one)
import Prelude(Double, Show)

data Ellipsoid =
   Ellipsoid
      (Length Double)         -- majorRadius
      (Dimensionless Double)  -- flatR
   deriving (Eq, Ord, Show)

class (GetEllipsoid a, ManyEllipsoid a) => HasEllipsoid a where
  ellipsoid ::
    Lens' a Ellipsoid
  majorRadius ::
    Lens' a (Length Double)
  {-# INLINE majorRadius #-}
  flatR ::
    Lens' a (Dimensionless Double)
  {-# INLINE flatR #-}
  majorRadius =
    ellipsoid . majorRadius
  flatR =  
    ellipsoid . flatR

instance HasEllipsoid Ellipsoid where
  ellipsoid =
    id
  majorRadius k (Ellipsoid r f) =
    fmap (\r' -> Ellipsoid r' f) (k r)
  {-# INLINE majorRadius #-}
  flatR k (Ellipsoid r f) =
    fmap (\f' -> Ellipsoid r f') (k f)
  {-# INLINE flatR #-}

class ManyEllipsoid a => AsEllipsoid a where
  _Ellipsoid ::
    Prism' a Ellipsoid
  _EllipsoidFields ::
    Prism' a (Length Double, Dimensionless Double)
  _EllipsoidFields =
    _Ellipsoid . _EllipsoidFields

instance AsEllipsoid Ellipsoid where
  _Ellipsoid =
    id
  _EllipsoidFields =
    prism
      (uncurry Ellipsoid)
      (\(Ellipsoid r f) -> Right (r, f))
  
class (FoldEllipsoid a, SetEllipsoid a) => ManyEllipsoid a where
  _ManyEllipsoid ::
    Traversal' a Ellipsoid

instance ManyEllipsoid Ellipsoid where
  _ManyEllipsoid =
    id

class FoldEllipsoid a => GetEllipsoid a where
  _GetEllipsoid ::
    Getter a Ellipsoid
    
instance GetEllipsoid Ellipsoid where
  _GetEllipsoid =
    id

class SetEllipsoid a where
  _SetEllipsoid ::
    Setter' a Ellipsoid
    
instance SetEllipsoid Ellipsoid where
  _SetEllipsoid =
    id

class FoldEllipsoid a where
  _FoldEllipsoid ::
    Fold a Ellipsoid
    
instance FoldEllipsoid Ellipsoid where
  _FoldEllipsoid =
    id

class (HasEllipsoid a, AsEllipsoid a) => IsEllipsoid a where
  _IsEllipsoid ::
    Iso' a Ellipsoid
    
instance IsEllipsoid Ellipsoid where
  _IsEllipsoid =
    id

_GRS80 ::
  Ellipsoid
_GRS80 =
  Ellipsoid
    (6378137.0 *~ meter)
    (294.25722100882711 *~ one)

_GRS67 ::
  Ellipsoid
_GRS67 =
  Ellipsoid
    (6378160 *~ meter)
    (298.25 *~ one)

_Ans ::
  Ellipsoid
_Ans =
  Ellipsoid
    (6378160 *~ meter)
    (298.25 *~ one)

_WGS72 ::
  Ellipsoid
_WGS72 =
  Ellipsoid
    (6378135 *~ meter)
    (298.26 *~ one)

_AU1965 ::
  Ellipsoid
_AU1965 =
  Ellipsoid
    (6378160 *~ meter)
    (298.25 *~ one)

_Krasovsky1940 ::
  Ellipsoid
_Krasovsky1940 =
  Ellipsoid
    (6378245 *~ meter)
    (298.3 *~ one)

_International1924 ::
  Ellipsoid
_International1924 =
  Ellipsoid
    (6378388 *~ meter)
    (297 *~ one)

_Hayford1909 ::
  Ellipsoid
_Hayford1909 =
  Ellipsoid
    (6378388 *~ meter)
    (297 *~ one)

_Airy1830 ::
  Ellipsoid
_Airy1830 =
  Ellipsoid
    (6377563.4 *~ meter)
    (299.32 *~ one)

_Everest1830 ::
  Ellipsoid
_Everest1830 =
  Ellipsoid
    (6377276.3 *~ meter)
    (300.8 *~ one)

_Clarke1858 ::
  Ellipsoid
_Clarke1858 =
  Ellipsoid
    (6378293.645 *~ meter)
    (294.26 *~ one)

_Clarke1880 ::
  Ellipsoid
_Clarke1880 =
  Ellipsoid
    (6378249.145 *~ meter)
    (293.465 *~ one)
