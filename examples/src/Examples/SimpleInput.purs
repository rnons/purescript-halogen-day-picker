module Examples.SimpleInput where

import Prelude

import Data.Date (Date)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.DayPicker (SelectedDate(..))
import Halogen.DayPicker as DayPicker
import Halogen.DayPickerInput as DPI
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Query a
  = HandlePicker DPI.Message a

type State =
  { selectedDate :: SelectedDate
  }

type Slot = (input :: H.Slot DPI.Query DPI.Message Unit)

_input = SProxy :: SProxy "input"

component :: forall m. MonadAff m => Date -> H.Component HH.HTML Query Unit Void m
component today = H.component
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  , initializer: Nothing
  , finalizer: Nothing
  }
  where

  initialState :: State
  initialState = { selectedDate: SelectedNone }

  render :: State -> H.ComponentHTML Query Slot m
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Simple day picker input" ]
      , HH.p_
          [ HH.text $ "Click input to show a calendar" ]
      , HH.slot _input unit
          DPI.dayPickerInput
          props
          (HE.input HandlePicker)
      ]
    where
    dayPickerProps = (DayPicker.defaultProps today) { selectedDate = state.selectedDate }
    value =
      case state.selectedDate of
        SelectedSingle date -> Just date
        _ -> Nothing
    props = (DPI.defaultProps dayPickerProps) { value = value }

  eval :: Query ~> H.HalogenM State Query Slot Void m
  eval (HandlePicker (DPI.Select date) next) = do
      H.modify_ $ _{ selectedDate = SelectedSingle date }
      pure next
  eval (HandlePicker (DPI.Input mDate) next) = do
      H.modify_ $ _{ selectedDate = maybe SelectedNone SelectedSingle mDate }
      pure next
