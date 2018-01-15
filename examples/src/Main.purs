module Main (main) where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW, nowDate)

import Data.DateTime.Locale (LocalValue(..))

import Routing (hashes)

import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import App (app, Query(RouteChange))

main :: Eff (HA.HalogenEffects (console :: CONSOLE, now :: NOW)) Unit
main = do
  LocalValue _ date <- nowDate
  HA.runHalogenAff do
    body <- HA.awaitBody
    app' <- runUI (app date) unit body
    liftEff $ hashes $ \_ next ->
      launchAff_ $ app'.query (H.action $ RouteChange next)
