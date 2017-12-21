module Examples.SimpleInput where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Date (Date)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Halogen.DayPickerInput as DayPickerInput

data Query a
  = HandleDayPicker DayPickerInput.Message a

type State =
  { selectedDate :: Maybe Date
  }

type Slot = Unit

component :: forall m. Date -> H.Component HH.HTML Query Unit Void m
component today =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { selectedDate: Nothing }

  render :: State -> H.ParentHTML Query DayPickerInput.Query Slot m
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Simple day picker input" ]
      , HH.p_
          [ HH.text $ "Click input to show a calendar" ]
      , HH.slot unit
          (DayPickerInput.dayPickerInput today)
          state.selectedDate
          (HE.input HandleDayPicker)
      ]

  eval :: Query ~> H.ParentDSL State Query DayPickerInput.Query Slot Void m
  eval = case _ of
    HandleDayPicker date next -> do
      H.modify $ _{ selectedDate = Just date }
      pure next
