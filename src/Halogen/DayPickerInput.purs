module Halogen.DayPickerInput where

import Prelude

import Control.Monad.Aff (Aff, delay, forkAff, liftEff')
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)

import Data.Enum (class BoundedEnum, fromEnum)
import Data.Date (Date)
import Data.Date as Date
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))

import DOM (DOM)
import DOM.Event.Types (Event)
import DOM.Event.EventTarget as Etr
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes as Etp
import DOM.HTML.Types (htmlDocumentToEventTarget, htmlElementToNode)
import DOM.HTML.HTMLElement (focus)
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
import Halogen.DayPickerInput.Styles (Styles, defaultStyles)

type Effects m
  = Aff ( dom :: DOM
        , avar :: AVAR
        , console :: CONSOLE | m)

type Props =
  { dayPickerProps :: DayPicker.Props
  , value :: Maybe Date
  , placeholder :: String
  , styles :: Styles
  , formatDate :: Date -> String
  }

type State =
  { dayPickerProps :: DayPicker.Props
  , focused :: Boolean
  , value :: Maybe Date
  , placeholder :: String
  , styles :: Styles
  , formatDate :: Date -> String
  }

data Query a
  = Init a
  | ClickDocument Event a
  | Focus a
  | OnFocus a
  | OnReceiveProps Props a
  | HandleDayPicker DayPicker.Message a

type Message = Date

type Slot = Unit

defaultFormatDate :: Date -> String
defaultFormatDate date =
  year <> "-" <> month <> "-" <> day
  where
  cs :: forall a. BoundedEnum a => a -> String
  cs = show <<< fromEnum
  year = cs $ Date.year date
  month = cs $ Date.month date
  day = cs $ Date.day date

defaultPropsFromDate :: Date -> Props
defaultPropsFromDate today =
  defaultProps dayPickerProps
  where
  dayPickerProps = DayPicker.defaultProps today

defaultProps :: DayPicker.Props -> Props
defaultProps dayPickerProps =
  { dayPickerProps: dayPickerProps
  , value: Nothing
  , placeholder: "YYYY-M-D"
  , styles: defaultStyles
  , formatDate: defaultFormatDate
  }

updateStateWithProps :: Props -> State -> State
updateStateWithProps { dayPickerProps, value, placeholder, styles, formatDate } =
  _{ dayPickerProps = dayPickerProps
   , value = value
   , placeholder = placeholder
   , styles = styles
   , formatDate = formatDate
   }

rootRef :: H.RefLabel
rootRef = H.RefLabel "root"

inputRef :: H.RefLabel
inputRef = H.RefLabel "input"

dayPickerInput :: forall m. H.Component HH.HTML Query Props Message (Effects m)
dayPickerInput = H.lifecycleParentComponent
  { initialState: initialState
  , render
  , eval
  , receiver: HE.input OnReceiveProps
  , initializer: Just $ H.action Init
  , finalizer: Nothing
  }
  where

  initialState :: Props -> State
  initialState { dayPickerProps, value, placeholder, styles, formatDate } =
    { dayPickerProps: dayPickerProps
    , focused: false
    , value: value
    , placeholder: placeholder
    , styles: styles
    , formatDate: formatDate
    }

  render :: State -> H.ParentHTML Query DayPicker.Query Unit (Effects m)
  render state@{ styles } =
    HH.div
      [ HP.class_ styles.root
      , HP.ref rootRef
      ]
      [ HH.input
          [ HP.type_ InputText
          , HP.class_ styles.input
          , HP.value value
          , HP.placeholder state.placeholder
          , HP.ref inputRef
          , HE.onFocus $ HE.input_ OnFocus
          ]
      , if state.focused
        then HH.div [ HP.class_ styles.dropdown ] [ dayPicker ]
        else HH.text ""
      ]
    where
    dayPicker =
      HH.slot unit DayPicker.dayPicker state.dayPickerProps (HE.input HandleDayPicker)
    value =
      case state.value of
        Nothing -> ""
        Just date -> state.formatDate date

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
    mInput <- H.getHTMLElementRef inputRef
    case mInput of
      Just input -> do
        _ <- H.liftAff $ forkAff $ do
          delay $ Milliseconds 10.0
          liftEff' $ focus input
        pure next
      Nothing -> pure next

  eval (OnFocus next) = do
    H.modify $ _{ focused = true }
    pure next

  eval (OnReceiveProps input next) = do
    H.modify $ updateStateWithProps input
    pure next

  eval (HandleDayPicker date next) = do
    H.modify $ _{ focused = false }
    H.raise date
    pure next
