module Examples.Multiple where

import Prelude

import Data.Date (Date)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Halogen as H
import Halogen.DayPicker as DayPicker
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Query a
  = HandleDayPicker DayPicker.Message a

type State =
  { today :: Date
  , selectedDate :: Set Date
  }

data Slot = DayPickerSlot
derive instance eqDayPickerSlot :: Eq Slot
derive instance ordDayPickerSlot :: Ord Slot


initialState :: Date -> State
initialState today =
  { today
  , selectedDate: Set.empty
  }

render :: forall m. State -> H.ParentHTML Query DayPicker.Query Slot m
render state =
  HH.div_
  [ HH.h1_
    [ HH.text "Select multiple days" ]
  , HH.p_
    [ HH.text $ "Click to select one or more days" ]
  , HH.div_
    [ HH.slot DayPickerSlot DayPicker.dayPicker props (HE.input HandleDayPicker) ]
  , HH.text $ "You selected " <> show state.selectedDate
  ]
  where
  props = (DayPicker.defaultProps state.today)
    { selectedDate = DayPicker.SelectedSet state.selectedDate }

component :: forall m. Date -> H.Component HH.HTML Query Unit Void m
component today = H.parentComponent
  { initialState: const $ initialState today
  , render
  , eval
  , receiver: const Nothing
  }
  where
  eval :: Query ~> H.ParentDSL State Query DayPicker.Query Slot Void m
  eval (HandleDayPicker date n) = n <$ do
    H.modify_ $ \s -> s
      { selectedDate =
          if Set.member date s.selectedDate
          then Set.delete date s.selectedDate
          else Set.insert date s.selectedDate
      }
