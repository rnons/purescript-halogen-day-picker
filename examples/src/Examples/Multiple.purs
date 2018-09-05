module Examples.Multiple where

import Prelude

import Data.Date (Date)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.DayPicker as DP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Query a
  = HandleDayPicker DP.Message a

type State =
  { today :: Date
  , selectedDate :: Set Date
  }

type Slot = (picker :: H.Slot DP.Query DP.Message Unit)

_picker = SProxy :: SProxy "picker"

initialState :: Date -> State
initialState today =
  { today
  , selectedDate: Set.empty
  }

render :: forall m. State -> H.ComponentHTML Query Slot m
render state =
  HH.div_
  [ HH.h1_
    [ HH.text "Select multiple days" ]
  , HH.p_
    [ HH.text $ "Click to select one or more days" ]
  , HH.div_
    [ HH.slot _picker unit DP.dayPicker props (HE.input HandleDayPicker) ]
  , HH.text $ "You selected " <> show state.selectedDate
  ]
  where
  props = (DP.defaultProps state.today)
    { selectedDate = DP.SelectedSet state.selectedDate }

component :: forall m. Date -> H.Component HH.HTML Query Unit Void m
component today = H.component
  { initialState: const $ initialState today
  , render
  , eval
  , receiver: const Nothing
  , initializer: Nothing
  , finalizer: Nothing
  }
  where
  eval :: Query ~> H.HalogenM State Query Slot Void m
  eval (HandleDayPicker date n) = n <$ do
    H.modify_ $ \s -> s
      { selectedDate =
          if Set.member date s.selectedDate
          then Set.delete date s.selectedDate
          else Set.insert date s.selectedDate
      }
