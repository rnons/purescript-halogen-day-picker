module Halogen.DayPickerInput where

import Prelude

import Data.Date (Date)
import Data.Maybe (Maybe)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import DOM.HTML.Indexed.InputType (InputType(InputText))

import Halogen.DayPicker as DayPicker

type Input =
  { dayPickerInput :: DayPicker.Input
  }

data Query a
  = HandleInput Input a
  | HandleDayPicker DayPicker.Message a

type State =
  { dayPickerInput :: DayPicker.Input
  }

type Message = Date

type Slot = Unit

dayPickerInput :: forall m. H.Component HH.HTML Query Input Message m
dayPickerInput =
  H.parentComponent
    { initialState: initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  initialState :: Input -> State
  initialState { dayPickerInput } =
    { dayPickerInput: dayPickerInput
    }

  render :: State -> H.ParentHTML Query DayPicker.Query Unit m
  render state =
      HH.div_
        [ HH.input [ HP.type_ InputText, HP.value $ show state.dayPickerInput.selectedDate ]
        , HH.slot unit DayPicker.dayPicker state.dayPickerInput (HE.input HandleDayPicker)
        ]

  eval :: Query ~> H.ParentDSL State Query DayPicker.Query Slot Message m
  eval (HandleInput { dayPickerInput } next) = do
    H.modify $ _{ dayPickerInput = dayPickerInput }
    pure next
  eval (HandleDayPicker date next) = do
    H.raise date
    pure next
