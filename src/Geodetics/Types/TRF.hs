{-# LANGUAGE NoImplicitPrelude #-}

module Geodetics.Types.TRF where

import Prelude
import Geodetics.Types.Ellipsoid
import Geodetics.Types.Helmert

data TRF =
   TRF
      Ellipsoid
      Helmert                 -- helmert
   deriving (Eq, Show)
