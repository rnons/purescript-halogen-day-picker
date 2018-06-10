module Examples.RangeWithTwoInputs where

import Prelude

import Data.Date (Date)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Examples.Utils (class_)
import Halogen as H
import Halogen.DayPicker (SelectedDate(FromTo), DisabledDate(..))
import Halogen.DayPicker as DayPicker
import Halogen.DayPickerInput as DayPickerInput
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

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
  initialState =
    { fromDate: Nothing
    , toDate: Nothing
    }

  render :: State -> H.ParentHTML Query DayPickerInput.Query Slot m
  render { fromDate, toDate } =
    HH.div
      [ class_ "example-range" ]
      [ HH.h1_
          [ HH.text "Select a range of dates with two inputs" ]
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
      , disabledDate = Before today
      }
    pickerFromProps =
      case toDate of
        Just to ->
          dayPickerProps
            { disabledDate = DayPicker.DisabledArray [Before today, After to] }
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

  eval :: Query ~> H.ParentDSL State Query DayPickerInput.Query Slot Void m
  eval (HandlePickerFrom (DayPickerInput.Select date) next) = do
      void $ H.modify $ _{ fromDate = Just date }
      _ <- H.query SlotTo $ H.action DayPickerInput.Focus
      pure next
  eval (HandlePickerFrom (DayPickerInput.Input mDate) next) = do
      void $ H.modify $ _{ fromDate = mDate }
      pure next
  eval (HandlePickerTo (DayPickerInput.Select date) next) = do
      void $ H.modify $ _{ toDate = Just date }
      pure next
  eval (HandlePickerTo (DayPickerInput.Input mDate) next) = do
      void $ H.modify $ _{ toDate = mDate }
      pure next
