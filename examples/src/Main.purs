module Main (main) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, nowDate)
import Data.DateTime.Locale (LocalValue(..))
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import App (app)

main :: Eff (HA.HalogenEffects (now :: NOW)) Unit
main = do
  LocalValue _ date <- nowDate
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI (app date) unit body
