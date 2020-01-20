module Control.Lens.MisoString where

import           Miso.String

import           Control.Lens (Iso', iso)

_MisoString :: ToMisoString a => Iso' MisoString a
_MisoString = iso fromMisoString toMisoString
