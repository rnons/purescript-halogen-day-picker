module Examples.Simple where

import Prelude

import Data.Date (Date)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.DayPicker as DP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Query a
  = HandleDayPicker DP.Message a

type State =
  { selectedDate :: DP.SelectedDate
  }

type Slot = (picker :: H.Slot DP.Query DP.Message Unit)

_picker = SProxy :: SProxy "picker"

component :: forall m. Date -> H.Component HH.HTML Query Unit Void m
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
  initialState = { selectedDate: DP.SelectedNone  }

  render :: State -> H.ComponentHTML Query Slot m
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Simple day picker" ]
      , HH.p_
          [ HH.text $ "Click to select a day" ]
      , HH.div_
          [ HH.slot _picker unit DP.dayPicker input (HE.input HandleDayPicker) ]
      , HH.text $ "You selected " <> show state.selectedDate
      ]
    where
    input = (DP.defaultProps today) { selectedDate = state.selectedDate }

  eval :: Query ~> H.HalogenM State Query Slot Void m
  eval = case _ of
    HandleDayPicker date next -> do
      H.modify_ (\state -> state { selectedDate = DP.SelectedSingle date })
      pure next
