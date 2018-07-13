{-# LANGUAGE NoImplicitPrelude #-}

module Geodetics.Types.Helmert where
  
import Prelude
import Numeric.Units.Dimensional.Prelude

-- | The 7 parameter Helmert transformation. The monoid instance allows composition.
data Helmert = Helmert
   (Length Double)
   (Length Double)
   (Length Double)
   (Dimensionless Double)  -- Parts per million
   (Dimensionless Double)
   (Dimensionless Double)
   (Dimensionless Double)
   deriving (Eq, Show)
