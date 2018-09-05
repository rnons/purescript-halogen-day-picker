module Examples.RangeWithTwoInputs where

import Prelude

import Data.Date (Date)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff.Class (class MonadAff)
import Examples.Utils (class_)
import Halogen as H
import Halogen.DayPicker (SelectedDate(SelectedRange), DisabledDate(..))
import Halogen.DayPicker as DP
import Halogen.DayPickerInput as DPI
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

data Query a
  = HandlePickerFrom DPI.Message a
  | HandlePickerTo DPI.Message a

type State =
  { fromDate :: Maybe Date
  , toDate :: Maybe Date
  }

type Slot = (input :: H.Slot DPI.Query DPI.Message String)

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
  initialState =
    { fromDate: Nothing
    , toDate: Nothing
    }

  render :: State -> H.ComponentHTML Query Slot m
  render { fromDate, toDate } =
    HH.div
      [ class_ "example-range" ]
      [ HH.h1_
          [ HH.text "Select a range of dates with two inputs" ]
      , HH.p_
          [ HH.text "Click input to show a calendar" ]
      , HH.div
          [ class_ "row"]
          [ HH.slot _input "from"
              DPI.dayPickerInput
              fromProps
              (HE.input HandlePickerFrom)
          , HH.slot _input "to"
              DPI.dayPickerInput
              toProps
              (HE.input HandlePickerTo)
          ]
      , HH.p_
          [ HH.text "hello world" ]
      ]
    where
    dayPickerProps = (DP.defaultProps today)
      { selectedDate = SelectedRange fromDate toDate
      , numberOfMonths = 2
      , disabledDate = DisabledBefore today
      }
    pickerFromProps =
      case toDate of
        Just to ->
          dayPickerProps
            { disabledDate = DP.DisabledArray [DisabledBefore today, DisabledAfter to] }
        _ -> dayPickerProps
    pickerToProps =
      case fromDate of
        Just from ->
          dayPickerProps { mode = DP.ToMode, disabledDate = DisabledBefore from }
        _ -> dayPickerProps { mode = DP.ToMode }
    fromProps =
      (DPI.defaultProps pickerFromProps)
        { placeholder = "FROM"
        , value = fromDate
        }
    toProps =
      (DPI.defaultProps pickerToProps)
        { placeholder = "TO"
        , value = toDate
        }

  eval :: Query ~> H.HalogenM State Query Slot Void m
  eval (HandlePickerFrom (DPI.Select date) next) = do
      H.modify_ $ _{ fromDate = Just date }
      _ <- H.query _input "to" $ H.action DPI.Focus
      pure next
  eval (HandlePickerFrom (DPI.Input mDate) next) = do
      H.modify_ $ _{ fromDate = mDate }
      pure next
  eval (HandlePickerTo (DPI.Select date) next) = do
      H.modify_ $ _{ toDate = Just date }
      pure next
  eval (HandlePickerTo (DPI.Input mDate) next) = do
      H.modify_ $ _{ toDate = mDate }
      pure next
