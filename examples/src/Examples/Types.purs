module Examples.Types
  ( Effects
  , AppEffects
  , AppM
  ) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW)
import Halogen.Aff (HalogenEffects)

type Effects =
  ( console :: CONSOLE
  , now :: NOW
  )

type AppEffects = HalogenEffects (Effects)

type AppM = Aff AppEffects
