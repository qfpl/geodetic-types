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
) where

import Control.Category((.), id)
import Control.Lens(Lens', Prism', Traversal', Getter, Setter', Fold, Iso', prism)
import Data.Either(Either(Right))
import Data.Eq(Eq)
import Data.Functor(fmap)
import Data.Tuple(uncurry)
import Numeric.Units.Dimensional.Prelude(Length, Dimensionless)
import Prelude(Double, Show)

data Ellipsoid =
   Ellipsoid
      (Length Double)         -- majorRadius
      (Dimensionless Double)  -- flatR
   deriving (Eq, Show)

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
