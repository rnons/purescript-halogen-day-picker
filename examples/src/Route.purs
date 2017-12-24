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
  | RangeWithTwoInputs

instance showRoute :: Show Route where
  show Home = ""
  show Simple = "#simple"
  show SimpleInput = "#simple-input"
  show RangeWithTwoInputs = "#range-with-two-inputs"

home :: Match Route
home = Home <$ lit ""

simple :: Match Route
simple = Simple <$ lit "simple"

simpleInput :: Match Route
simpleInput = SimpleInput <$ lit "simple-input"

rangeWithTwoInputs :: Match Route
rangeWithTwoInputs = RangeWithTwoInputs <$ lit "range-with-two-inputs"

routing :: Match Route
routing =
  simple <|>
  simpleInput <|>
  rangeWithTwoInputs <|>
  home
