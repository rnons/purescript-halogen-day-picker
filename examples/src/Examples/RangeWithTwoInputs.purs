module Examples.RangeWithTwoInputs where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Date (Date)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Halogen.DayPicker (SelectedDate(FromTo), DisabledDate(..))
import Halogen.DayPicker as DayPicker
import Halogen.DayPickerInput as DayPickerInput

import Examples.Utils (class_)
import Examples.Types (AppM)

data Query a
  = HandlePickerFrom DayPickerInput.Message a
  | HandlePickerTo DayPickerInput.Message a

type State =
  { fromDate :: Maybe Date
  , toDate :: Maybe Date
  }

data Slot = SlotFrom | SlotTo
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot


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
  initialState =
    { fromDate: Nothing
    , toDate: Nothing
    }

  render :: State -> H.ParentHTML Query DayPickerInput.Query Slot AppM
  render { fromDate, toDate } =
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
      { selectedDate = FromTo fromDate toDate
      , numberOfMonths = 2
      }
    pickerFromProps =
      case toDate of
        Just to ->
          dayPickerProps { disabledDate = After to }
        _ -> dayPickerProps
    pickerToProps =
      case fromDate of
        Just from ->
          dayPickerProps { disabledDate = Before from }
        _ -> dayPickerProps
    fromProps =
      (DayPickerInput.defaultProps pickerFromProps)
        { placeholder = "FROM"
        , value = fromDate
        }
    toProps =
      (DayPickerInput.defaultProps pickerToProps)
        { placeholder = "TO"
        , value = toDate
        }

  eval :: Query ~> H.ParentDSL State Query DayPickerInput.Query Slot Void AppM
  eval (HandlePickerFrom (DayPickerInput.Select date) next) = do
      H.modify $ _{ fromDate = Just date }
      _ <- H.query SlotTo $ H.action DayPickerInput.Focus
      pure next
  eval (HandlePickerFrom (DayPickerInput.Input mDate) next) = do
      H.modify $ _{ fromDate = mDate }
      pure next
  eval (HandlePickerTo (DayPickerInput.Select date) next) = do
      H.modify $ _{ toDate = Just date }
      pure next
  eval (HandlePickerTo (DayPickerInput.Input mDate) next) = do
      H.modify $ _{ toDate = mDate }
      pure next
