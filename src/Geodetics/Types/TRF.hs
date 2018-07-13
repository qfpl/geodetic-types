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
) where

import Control.Category((.), id)
import Control.Lens(Lens', Prism', Traversal', Getter, Setter', Fold, Iso', prism)
import Data.Either(Either(Right))
import Data.Eq(Eq)
import Data.Ord(Ord)
import Data.Functor(fmap)
import Data.Tuple(uncurry)
import Geodetics.Types.Ellipsoid(Ellipsoid, HasEllipsoid(ellipsoid), GetEllipsoid(_GetEllipsoid), ManyEllipsoid(_ManyEllipsoid), FoldEllipsoid(_FoldEllipsoid), SetEllipsoid(_SetEllipsoid))
import Geodetics.Types.Helmert(Helmert, HasHelmert(helmert), GetHelmert(_GetHelmert), ManyHelmert(_ManyHelmert), FoldHelmert(_FoldHelmert), SetHelmert(_SetHelmert))
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
