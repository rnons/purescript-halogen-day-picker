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
import Examples.Utils (class_)

data Query a
  = HandlePickerFrom DayPickerInput.Message a
  | HandlePickerTo DayPickerInput.Message a

type State =
  { selectedDate :: DayPicker.SelectedDate
  }

data Slot = SlotFrom | SlotTo
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


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
          [ HH.text "Click input to show a calendar" ]
      , HH.div
          [ class_ "row"]
          [ HH.slot SlotFrom
              DayPickerInput.dayPickerInput
              input
              (HE.input HandlePickerFrom)
          , HH.slot SlotTo
              DayPickerInput.dayPickerInput
              input
              (HE.input HandlePickerTo)
          ]
      , HH.p_
          [ HH.text "hello world" ]
      ]
    where
    dayPickerInput = (DayPicker.defaultInput today)
              { selectedDate = state.selectedDate
              , numberOfMonths = 2
              }
    input = { dayPickerInput: dayPickerInput }

  eval :: Query ~> H.ParentDSL State Query DayPickerInput.Query Slot Void (Effects m)
  eval = case _ of
    HandlePickerFrom date next -> do
      H.modify $ \state@{ selectedDate } ->
        case selectedDate of
          DayPicker.FromTo _ to ->
            state { selectedDate = DayPicker.FromTo (Just date) to }
          _ ->
            state { selectedDate = DayPicker.FromTo (Just date) Nothing }
      _ <- H.query SlotTo $ H.action DayPickerInput.Focus
      pure next
    HandlePickerTo date next -> do
      H.modify $ \state@{ selectedDate } ->
        case selectedDate of
          DayPicker.FromTo from _ ->
            state { selectedDate = DayPicker.FromTo from (Just date) }
          _ ->
            state { selectedDate = DayPicker.FromTo Nothing (Just date) }
      pure next
