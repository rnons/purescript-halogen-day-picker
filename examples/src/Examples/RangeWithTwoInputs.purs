module Examples.RangeWithTwoInputs where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Date (Date)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Halogen.DayPicker (SelectedDate(..), DisabledDate(..))
import Halogen.DayPicker as DayPicker
import Halogen.DayPickerInput (Effects)
import Halogen.DayPickerInput as DayPickerInput
import Examples.Utils (class_)

data Query a
  = HandlePickerFrom DayPickerInput.Message a
  | HandlePickerTo DayPickerInput.Message a

type State =
  { selectedDate :: SelectedDate
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
  initialState = { selectedDate: NoneSelected }

  render :: State -> H.ParentHTML Query DayPickerInput.Query Slot (Effects m)
  render state =
    HH.div
      [ class_ "example-range" ]
      [ HH.h1_
          [ HH.text "Simple day picker input" ]
      , HH.p_
          [ HH.text "Click input to show a calendar" ]
      , HH.div
          [ class_ "row"]
          [ HH.slot SlotFrom
              DayPickerInput.dayPickerInput
              fromProps
              (HE.input HandlePickerFrom)
          , HH.slot SlotTo
              DayPickerInput.dayPickerInput
              toProps
              (HE.input HandlePickerTo)
          ]
      , HH.p_
          [ HH.text "hello world" ]
      ]
    where
    dayPickerProps = (DayPicker.defaultProps today)
              { selectedDate = state.selectedDate
              , numberOfMonths = 2
              }
    pickerFromProps =
      case state.selectedDate of
        FromTo _ (Just to) ->
          dayPickerProps { disabledDate = After to }
        _ -> dayPickerProps
    pickerToProps =
      case state.selectedDate of
        FromTo (Just from) _ ->
          dayPickerProps { disabledDate = Before from }
        _ -> dayPickerProps
    getFrom =
      case _ of
        FromTo from _ -> from
        _ -> Nothing
    getTo =
      case _ of
        FromTo _ to -> to
        _ -> Nothing
    fromProps =
      (DayPickerInput.defaultProps pickerFromProps)
        { placeholder = "FROM"
        , value = getFrom state.selectedDate
        }
    toProps =
      (DayPickerInput.defaultProps pickerToProps)
        { placeholder = "TO"
        , value = getTo state.selectedDate
        }

  eval :: Query ~> H.ParentDSL State Query DayPickerInput.Query Slot Void (Effects m)
  eval = case _ of
    HandlePickerFrom date next -> do
      H.modify $ \state@{ selectedDate } ->
        case selectedDate of
          FromTo _ to ->
            state { selectedDate = FromTo (Just date) to }
          _ ->
            state { selectedDate = FromTo (Just date) Nothing }
      _ <- H.query SlotTo $ H.action DayPickerInput.Focus
      pure next
    HandlePickerTo date next -> do
      H.modify $ \state@{ selectedDate } ->
        case selectedDate of
          FromTo from _ ->
            state { selectedDate = FromTo from (Just date) }
          _ ->
            state { selectedDate = FromTo Nothing (Just date) }
      pure next
