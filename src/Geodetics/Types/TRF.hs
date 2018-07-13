{-# LANGUAGE NoImplicitPrelude #-}

module Geodetics.Types.TRF(
  TRF(..)
, HasTRF(..)
, AsTRF(..)
, ManyTRF(..)
, GetTRF(..)
, SetTRF(..)
, FoldTRF(..)
, IsTRF(..)
, _WGS84
, _OSGB36
, _Clarke1866
, _Bessel1841
) where

import Control.Category((.), id)
import Control.Lens(Lens', Prism', Traversal', Getter, Setter', Fold, Iso', prism)
import Data.Either(Either(Right))
import Data.Eq(Eq)
import Data.Monoid(mempty)
import Data.Ord(Ord)
import Data.Functor(fmap)
import Data.Tuple(uncurry)
import Geodetics.Types.Ellipsoid(Ellipsoid(Ellipsoid), HasEllipsoid(ellipsoid), GetEllipsoid(_GetEllipsoid), ManyEllipsoid(_ManyEllipsoid), FoldEllipsoid(_FoldEllipsoid), SetEllipsoid(_SetEllipsoid))
import Geodetics.Types.Helmert(Helmert(Helmert), HasHelmert(helmert), GetHelmert(_GetHelmert), ManyHelmert(_ManyHelmert), FoldHelmert(_FoldHelmert), SetHelmert(_SetHelmert))
import Numeric.Units.Dimensional.Prelude((*~), meter, one, arcsecond)
import Prelude(Show)

data TRF =
  TRF
    Ellipsoid
    Helmert
  deriving (Eq, Ord, Show)

instance HasEllipsoid TRF where
  ellipsoid k (TRF e h) =
    fmap (\e' -> TRF e' h) (k e)

instance GetEllipsoid TRF where
  _GetEllipsoid =
    ellipsoid

instance ManyEllipsoid TRF where
  _ManyEllipsoid =
    ellipsoid

instance FoldEllipsoid TRF where
  _FoldEllipsoid =
    ellipsoid

instance SetEllipsoid TRF where
  _SetEllipsoid =
    ellipsoid

instance HasHelmert TRF where
  helmert k (TRF e h) =
    fmap (\h' -> TRF e h') (k h)

instance GetHelmert TRF where
  _GetHelmert =
    helmert

instance ManyHelmert TRF where
  _ManyHelmert =
    helmert

instance FoldHelmert TRF where
  _FoldHelmert =
    helmert

instance SetHelmert TRF where
  _SetHelmert =
    helmert

class (GetTRF a, ManyTRF a, HasEllipsoid a) => HasTRF a where
  trf ::
    Lens' a TRF
    
instance HasTRF TRF where
  trf =
    id

class ManyTRF a => AsTRF a where
  _TRF ::
    Prism' a TRF
  _TRFFields ::
    Prism' a (Ellipsoid, Helmert)
  _TRFFields =
    _TRF . _TRFFields

instance AsTRF TRF where
  _TRF =
    id
  _TRFFields =
    prism
      (uncurry TRF)
      (\(TRF e h) -> Right (e, h))

class (FoldTRF a, SetTRF a) => ManyTRF a where
  _ManyTRF ::
    Traversal' a TRF

instance ManyTRF TRF where
  _ManyTRF =
    id

class FoldTRF a => GetTRF a where
  _GetTRF ::
    Getter a TRF
    
instance GetTRF TRF where
  _GetTRF =
    id

class SetTRF a where
  _SetTRF ::
    Setter' a TRF
    
instance SetTRF TRF where
  _SetTRF =
    id

class FoldTRF a where
  _FoldTRF ::
    Fold a TRF
    
instance FoldTRF TRF where
  _FoldTRF =
    id

class (HasTRF a, AsTRF a) => IsTRF a where
  _IsTRF ::
    Iso' a TRF
    
instance IsTRF TRF where
  _IsTRF =
    id

_WGS84 ::
  TRF
_WGS84 =
  TRF
    (
      Ellipsoid
        (6378137.0 *~ meter)
        (298.257223563 *~ one)
    )
    mempty

_OSGB36 ::
  TRF
_OSGB36 =
  TRF
    (
      Ellipsoid
        (6377563.396 *~ meter)
        (299.3249646 *~ one)
    )
    (
      Helmert 
        (446.448 *~ meter)
        ((-125.157) *~ meter)
        (542.06 *~ meter)
        ((-20.4894) *~ one)
        (0.1502 *~ arcsecond)
        (0.247 *~ arcsecond)
        (0.8421 *~ arcsecond)
    )

_Clarke1866 ::
  TRF
_Clarke1866 =
  TRF
    (
      Ellipsoid
        (6378206.4 *~ meter)
        (294.978698214 *~ one)
    )
    (
      Helmert 
        ((-8) *~ meter)
        (160 *~ meter)
        (176 *~ meter)
        (0 *~ one)
        (0 *~ arcsecond)
        (0 *~ arcsecond)
        (0 *~ arcsecond)
    )

_Bessel1841 ::
  TRF
_Bessel1841 =
  TRF
    (
      Ellipsoid
        (6377397.155 *~ meter)
        (299.1528153513233 *~ one)
    )
    (
      Helmert 
        (582 *~ meter)
        (105 *~ meter)
        (414 *~ meter)
        (8.3 *~ one)
        (1.04 *~ arcsecond)
        (0.35 *~ arcsecond)
        (3.08 *~ arcsecond)
    )
