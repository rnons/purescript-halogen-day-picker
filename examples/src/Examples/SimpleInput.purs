module Examples.SimpleInput where

import Prelude

import Data.Date (Date)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.DayPicker (SelectedDate(..))
import Halogen.DayPicker as DayPicker
import Halogen.DayPickerInput as DayPickerInput
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Query a
  = HandlePicker DayPickerInput.Message a

type State =
  { selectedDate :: SelectedDate
  }

type Slot = Unit

component :: forall m. MonadAff m => Date -> H.Component HH.HTML Query Unit Void m
component today =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { selectedDate: SelectedNone }

  render :: State -> H.ParentHTML Query DayPickerInput.Query Slot m
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
        SelectedSingle date -> Just date
        _ -> Nothing
    props = (DayPickerInput.defaultProps dayPickerProps) { value = value }

  eval :: Query ~> H.ParentDSL State Query DayPickerInput.Query Slot Void m
  eval (HandlePicker (DayPickerInput.Select date) next) = do
      H.modify_ $ _{ selectedDate = SelectedSingle date }
      pure next
  eval (HandlePicker (DayPickerInput.Input mDate) next) = do
      H.modify_ $ _{ selectedDate = maybe SelectedNone SelectedSingle mDate }
      pure next
