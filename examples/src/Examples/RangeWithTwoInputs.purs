module Examples.RangeWithTwoInputs where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Date (Date)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Halogen.DayPicker as DayPicker
import Halogen.DayPickerInput (Effects)
import Halogen.DayPickerInput as DayPickerInput

data Query a
  = HandleDayPicker DayPickerInput.Message a

type State =
  { selectedDate :: DayPicker.SelectedDate
  }

type Slot = Unit

component :: forall m. Date -> H.Component HH.HTML Query Unit Void (Effects m)
component today =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { selectedDate: DayPicker.None }

  render :: State -> H.ParentHTML Query DayPickerInput.Query Slot (Effects m)
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Simple day picker input" ]
      , HH.p_
          [ HH.text $ "Click input to show a calendar" ]
      , HH.slot unit
          DayPickerInput.dayPickerInput
          input
          (HE.input HandleDayPicker)
      ]
    where
    dayPickerInput = (DayPicker.defaultInput today)
              { selectedDate = state.selectedDate
              , numberOfMonths = 2
              }
    input = { dayPickerInput: dayPickerInput }

  eval :: Query ~> H.ParentDSL State Query DayPickerInput.Query Slot Void (Effects m)
  eval = case _ of
    HandleDayPicker date next -> do
      H.modify $ _{ selectedDate = DayPicker.Single date }
      pure next
