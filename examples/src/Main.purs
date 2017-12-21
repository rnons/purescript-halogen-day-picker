module Main (main) where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW, nowDate)
import Data.DateTime.Locale (LocalValue(..))

import Routing (matches)

import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Route (routing)
import App (app, Query(RouteChange))


main :: Eff (HA.HalogenEffects (now :: NOW)) Unit
main = do
  LocalValue _ date <- nowDate
  HA.runHalogenAff do
    body <- HA.awaitBody
    app' <- runUI (app date) unit body
    liftEff $ matches routing $ \_ next ->
      launchAff_ $ app'.query (H.action $ RouteChange next)
