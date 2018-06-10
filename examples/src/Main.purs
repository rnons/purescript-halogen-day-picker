module Main (main) where

import Prelude

import Data.Date (Date)
import Data.Tuple (Tuple(Tuple))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Now (nowDate)
import Examples.RangeWithTwoInputs as ExpRangeInputs
import Examples.Simple as ExpSimple
import Examples.SimpleInput as ExpSimpleInput
import Foreign.Object as Object
import Halogen.Aff as HA
import Halogen.Storybook (Stories, runStorybook, proxy)

stories :: forall m. MonadAff m => Date -> Stories m
stories today = Object.fromFoldable
  [ Tuple "Simple day picker" $ proxy $ ExpSimple.component today
  , Tuple "Simple day picker input" $ proxy $ ExpSimpleInput.component today
  , Tuple "Range with two inputs" $ proxy $ ExpRangeInputs.component today
  ]

main :: Effect Unit
main = do
  today <- nowDate
  HA.runHalogenAff do
    body <- HA.awaitBody
    runStorybook (stories today) body
