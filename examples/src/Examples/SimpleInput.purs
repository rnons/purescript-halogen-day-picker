module Examples.SimpleInput where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Data.Date (Date)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Halogen.DayPicker (SelectedDate(..))
import Halogen.DayPicker as DayPicker
import Halogen.DayPickerInput as DayPickerInput

import Examples.Types (AppM)

data Query a
  = HandlePicker DayPickerInput.Message a

type State =
  { selectedDate :: SelectedDate
  }

type Slot = Unit

component :: Date -> H.Component HH.HTML Query Unit Void AppM
component today =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { selectedDate: NoneSelected }

  render :: State -> H.ParentHTML Query DayPickerInput.Query Slot AppM
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Simple day picker input" ]
      , HH.p_
          [ HH.text $ "Click input to show a calendar" ]
      , HH.slot unit
          DayPickerInput.dayPickerInput
          props
          (HE.input HandlePicker)
      ]
    where
    dayPickerProps = (DayPicker.defaultProps today) { selectedDate = state.selectedDate }
    value =
      case state.selectedDate of
        Single date -> Just date
        _ -> Nothing
    props = (DayPickerInput.defaultProps dayPickerProps) { value = value }

  eval :: Query ~> H.ParentDSL State Query DayPickerInput.Query Slot Void AppM
  eval (HandlePicker (DayPickerInput.Select date) next) = do
      H.modify $ _{ selectedDate = Single date }
      pure next
  eval (HandlePicker (DayPickerInput.Input mDate) next) = do
      H.modify $ _{ selectedDate = maybe NoneSelected Single mDate }
      pure next
