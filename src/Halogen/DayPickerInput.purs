module Halogen.DayPickerInput where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Date (Date)
import Data.Maybe (Maybe(..))

import DOM (DOM)
import DOM.Event.Types (Event)
import DOM.Event.EventTarget as Etr
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes as Etp
import DOM.HTML.Types (htmlDocumentToEventTarget, htmlElementToNode)
import DOM.HTML.Indexed.InputType (InputType(InputText))
import DOM.HTML.Window (document)
import DOM.Node.Node (contains, isEqualNode)

import DOM.Classy.Event (target)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource as ES

import Halogen.DayPicker as DayPicker

type Effects m
  = Aff ( dom :: DOM
        , avar :: AVAR
        , console :: CONSOLE | m)

type Input =
  { dayPickerInput :: DayPicker.Input
  }

data Query a
  = Init a
  | ClickDocument Event a
  | Focus a
  | HandleInput Input a
  | HandleDayPicker DayPicker.Message a

type State =
  { dayPickerInput :: DayPicker.Input
  , focused :: Boolean
  }

type Message = Date

type Slot = Unit

rootRef :: H.RefLabel
rootRef = H.RefLabel "root"

dayPickerInput :: forall m. H.Component HH.HTML Query Input Message (Effects m)
dayPickerInput = H.lifecycleParentComponent
  { initialState: initialState
  , render
  , eval
  , receiver: HE.input HandleInput
  , initializer: Just $ H.action Init
  , finalizer: Nothing
  }
  where

  initialState :: Input -> State
  initialState { dayPickerInput } =
    { dayPickerInput: dayPickerInput
    , focused: false
    }

  render :: State -> H.ParentHTML Query DayPicker.Query Unit (Effects m)
  render state =
    HH.div
      [ HP.ref rootRef ]
      [ HH.input
          [ HP.type_ InputText
          , HP.value $ show state.dayPickerInput.selectedDate
          , HE.onFocus $ HE.input_ Focus
          ]
      , if state.focused then dayPicker else HH.text ""
      ]
    where
    dayPicker =
      HH.slot unit DayPicker.dayPicker state.dayPickerInput (HE.input HandleDayPicker)

  eval :: Query ~> H.ParentDSL State Query DayPicker.Query Slot Message (Effects m)
  eval (Init next) = do
    doc <- H.liftEff $ window >>= document
    let docTarget = htmlDocumentToEventTarget doc
        bindClick f =
          Etr.addEventListener Etp.click (Etr.eventListener f) false docTarget
        handleClick e = do
          pure $ (ClickDocument e) ES.Listening

    H.subscribe $ H.eventSource bindClick handleClick
    pure next

  eval (ClickDocument event next) = do
    let targetNode = target event
    mRootNode <- liftM1 htmlElementToNode <$> H.getHTMLElementRef rootRef
    case mRootNode of
      Just rootNode -> do
        isEqual <- H.liftEff $ isEqualNode rootNode targetNode
        isChild <- H.liftEff $ contains rootNode targetNode
        when (not isEqual && not isChild) $ H.modify $ _{ focused = false }
        pure next
      Nothing -> pure next

  eval (Focus next) = do
    H.modify $ _{ focused = true }
    pure next
  eval (HandleInput { dayPickerInput } next) = do
    H.modify $ _{ dayPickerInput = dayPickerInput }
    pure next
  eval (HandleDayPicker date next) = do
    H.raise date
    pure next
