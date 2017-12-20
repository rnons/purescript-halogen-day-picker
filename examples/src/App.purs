module App where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Date (Date)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Halogen.DayPicker as DayPicker

data Query a
  = HandleDayPicker DayPicker.Message a

type State =
  { selectedDate :: Maybe Date
  }

data Slot = DayPickerSlot
derive instance eqDayPickerSlot :: Eq Slot
derive instance ordDayPickerSlot :: Ord Slot

app :: forall m. Date -> H.Component HH.HTML Query Unit Void m
app today =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { selectedDate: Nothing }

  render :: State -> H.ParentHTML Query DayPicker.Query Slot m
  render state =
    HH.div_
      [ HH.h1_
          [ HH.text "Simple day picker" ]
      , HH.p_
          [ HH.text $ "Click to select a day" ]
      , HH.slot DayPickerSlot DayPicker.dayPicker today (HE.input HandleDayPicker)
      , HH.text $ "You selected " <> show state.selectedDate
      ]

  eval :: Query ~> H.ParentDSL State Query DayPicker.Query Slot Void m
  eval = case _ of
    HandleDayPicker date next -> do
      H.modify (\state -> state { selectedDate = Just date })
      pure next
