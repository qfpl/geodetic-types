{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Geodetics.Types.Altitude where
  
import Control.Lens(Lens', set)
import Data.Function(id)
import Numeric.Units.Dimensional.Prelude(Length, _0)
import Prelude(Double)

class HasAltitude a where
  altitude ::
    Lens' a (Length Double)

instance HasAltitude (Length Double) where
  altitude =
    id

groundPosition ::
  HasAltitude a =>
  a
  -> a
groundPosition =
  set altitude _0
