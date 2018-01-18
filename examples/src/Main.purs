module Main (main) where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (nowDate)

import Data.Date (Date)
import Data.DateTime.Locale (LocalValue(..))
import Data.StrMap as SM
import Data.Tuple (Tuple(Tuple))

import Halogen.Aff as HA
import Halogen.Storybook (Stories, runStorybook, proxy)

import Examples.Simple as ExpSimple
import Examples.SimpleInput as ExpSimpleInput
import Examples.RangeWithTwoInputs as ExpRangeInputs
import Examples.Types (AppM, AppEffects)

stories :: Date -> Stories AppM
stories today = SM.fromFoldable
  [ Tuple "Simple day picker" $ proxy $ ExpSimple.component today
  , Tuple "Simple day picker input" $ proxy $ ExpSimpleInput.component today
  , Tuple "Range with two inputs" $ proxy $ ExpRangeInputs.component today
  ]

main :: Eff AppEffects Unit
main = do
  LocalValue _ today <- nowDate
  HA.runHalogenAff do
    body <- HA.awaitBody
    runStorybook (stories today) body
