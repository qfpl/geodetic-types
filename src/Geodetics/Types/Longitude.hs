{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Geodetics.Types.Longitude where

import Control.Lens(Lens')
import Data.Function(id)
import Numeric.Units.Dimensional.Prelude(Angle)
import Prelude(Double)

class HasLongitude a where
  longitude ::
    Lens' a (Angle Double)

instance HasLongitude (Angle Double) where
  longitude =
    id
