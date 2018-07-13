{-# LANGUAGE NoImplicitPrelude #-}

module Geodetics.Types.Helmert(
  Helmert(..)
, HasHelmert(..)
, AsHelmert(..)
, ManyHelmert(..)
, GetHelmert(..)
, SetHelmert(..)
, FoldHelmert(..)
, IsHelmert(..)
) where

import Control.Category((.), id)
import Control.Lens(Lens', Prism', Traversal', Getter, Setter', Fold, Iso', prism)
import Data.Either(Either(Right))
import Data.Eq(Eq)
import Data.Ord(Ord)
import Data.Functor(fmap)
import Numeric.Units.Dimensional.Prelude(Length, Dimensionless)
import Prelude(Double, Show)

-- | The 7 parameter Helmert transformation. The monoid instance allows composition.
data Helmert =
  Helmert
    (Length Double)
    (Length Double)
    (Length Double)
    (Dimensionless Double)  -- Parts per million
    (Dimensionless Double)
    (Dimensionless Double)
    (Dimensionless Double)
  deriving (Eq, Ord, Show)

class HasHelmert a where
  helmert ::
    Lens' a Helmert
  cX ::
    Lens' a (Length Double)
  {-# INLINE cX #-}
  cY ::
    Lens' a (Length Double)
  {-# INLINE cY #-}
  cZ ::
    Lens' a (Length Double)
  {-# INLINE cZ #-}
  helmertScale ::
    Lens' a (Dimensionless Double)
  {-# INLINE helmertScale #-}
  rX ::
    Lens' a (Dimensionless Double)
  {-# INLINE rX #-}
  rY ::
    Lens' a (Dimensionless Double)
  {-# INLINE rY #-}
  rZ ::
    Lens' a (Dimensionless Double)
  {-# INLINE rZ #-}
  cX =
    helmert . cX
  cY =
     helmert . cY
  cZ =
    helmert . cZ
  helmertScale =
    helmert . helmertScale
  rX =
    helmert . rX
  rY =
    helmert . rY
  rZ =
    helmert . rZ

instance HasHelmert Helmert where
  {-# INLINE cX #-}
  {-# INLINE cY #-}
  {-# INLINE cZ #-}
  {-# INLINE helmertScale #-}
  {-# INLINE rX #-}
  {-# INLINE rY #-}
  {-# INLINE rZ #-}
  helmert =
    id
  cX k (Helmert cX' cY' cZ' helmertScale' rX' rY' rZ') =
    fmap (\x -> Helmert x cY' cZ' helmertScale' rX' rY' rZ') (k cX')
  cY k (Helmert cX' cY' cZ' helmertScale' rX' rY' rZ') =
    fmap (\x -> Helmert cX' x cZ' helmertScale' rX' rY' rZ') (k cY')
  cZ k (Helmert cX' cY' cZ' helmertScale' rX' rY' rZ') =
    fmap (\x -> Helmert cX' cY' x helmertScale' rX' rY' rZ') (k cZ')
  helmertScale k (Helmert cX' cY' cZ' helmertScale' rX' rY' rZ') =
    fmap (\x -> Helmert cX' cY' cZ' x rX' rY' rZ') (k helmertScale')
  rX k (Helmert cX' cY' cZ' helmertScale' rX' rY' rZ') =
    fmap (\x -> Helmert cX' cY' cZ' helmertScale' x rY' rZ') (k rX')
  rY k (Helmert cX' cY' cZ' helmertScale' rX' rY' rZ') =
    fmap (\x -> Helmert cX' cY' cZ' helmertScale' rX' x rZ') (k rY')
  rZ k (Helmert cX' cY' cZ' helmertScale' rX' rY' rZ') =
    fmap (\x -> Helmert cX' cY' cZ' helmertScale' rX' rY' x) (k rZ')

class ManyHelmert a => AsHelmert a where
  _Helmert ::
    Prism' a Helmert

  _HelmertFields ::
    Prism' a (Length Double, Length Double, Length Double, Dimensionless Double, Dimensionless Double, Dimensionless Double, Dimensionless Double)
  _HelmertFields =
    _Helmert . _HelmertFields

instance AsHelmert Helmert where
  _Helmert =
    id

  _HelmertFields =
    prism
      (\(cX', cY', cZ', helmertScale', rX', rY', rZ') -> Helmert cX' cY' cZ' helmertScale' rX' rY' rZ')
      (\(Helmert cX' cY' cZ' helmertScale' rX' rY' rZ') -> Right (cX', cY', cZ', helmertScale', rX', rY', rZ'))

class (FoldHelmert a, SetHelmert a) => ManyHelmert a where
  _ManyHelmert ::
    Traversal' a Helmert

instance ManyHelmert Helmert where
  _ManyHelmert =
    id

class FoldHelmert a => GetHelmert a where
  _GetHelmert ::
    Getter a Helmert
    
instance GetHelmert Helmert where
  _GetHelmert =
    id

class SetHelmert a where
  _SetHelmert ::
    Setter' a Helmert
    
instance SetHelmert Helmert where
  _SetHelmert =
    id

class FoldHelmert a where
  _FoldHelmert ::
    Fold a Helmert
    
instance FoldHelmert Helmert where
  _FoldHelmert =
    id

class (HasHelmert a, AsHelmert a) => IsHelmert a where
  _IsHelmert ::
    Iso' a Helmert
    
instance IsHelmert Helmert where
  _IsHelmert =
    id
