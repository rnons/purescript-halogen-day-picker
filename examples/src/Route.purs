module Route
  ( Route(..)
  , routing
  ) where

import Prelude

import Control.Alt ((<|>))

import Routing.Match (Match)
import Routing.Match.Class (lit)

data Route
  = Home
  | Simple
  | SimpleInput

instance showRoute :: Show Route where
  show Home = ""
  show Simple = "#simple"
  show SimpleInput = "#simple-input"

home :: Match Route
home = Home <$ lit ""

simple :: Match Route
simple = Simple <$ lit "simple"

simpleInput :: Match Route
simpleInput = SimpleInput <$ lit "simple-input"

routing :: Match Route
routing =
  simple <|>
  simpleInput <|>
  home
