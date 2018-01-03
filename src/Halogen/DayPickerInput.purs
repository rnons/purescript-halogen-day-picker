-- | A component containing a text input and a dayPicker. The dayPicker is
-- | wrapped in a dropdown.
module Halogen.DayPickerInput
  ( Props
  , Query(..)
  , Message(..)
  , defaultProps
  , dayPickerInput
  , module Halogen.DayPickerInput.Styles
  ) where

import Prelude

import Control.Monad.Aff (delay, forkAff, liftEff')
import Control.Monad.Aff.Class (class MonadAff)

import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Date (Date)
import Data.Date as Date
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(Pattern), split)
import Data.Traversable (sequence)
import Data.Time.Duration (Milliseconds(..))

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
import Halogen.Aff (HalogenEffects)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import Halogen.Query.EventSource as ES

import Halogen.DayPicker as DayPicker
import Halogen.DayPickerInput.Styles (Styles, defaultStyles)

defaultFormatDate :: Date -> String
defaultFormatDate date =
  year <> "-" <> month <> "-" <> day
  where
    cs :: forall a. BoundedEnum a => a -> String
    cs = show <<< fromEnum
    year = cs $ Date.year date
    month = cs $ Date.month date
    day = cs $ Date.day date

defaultParseDate :: String -> Maybe Date
defaultParseDate str =
  case mParts of
    Just [y, m, d] ->
      fromMaybe Nothing (Date.exactDate <$> (toEnum y) <*> (toEnum m) <*> (toEnum d))
    _ -> Nothing
  where
    mParts = sequence $ Int.fromString <$> split (Pattern "-") str

-- | What to show in the input and how to parse input value can be customized.
type Props =
  { dayPickerProps :: DayPicker.Props
  , value :: Maybe Date
  , placeholder :: String
  , styles :: Styles
  , formatDate :: Date -> String
  , parseDate :: String -> Maybe Date
  }

defaultPropsFromDate :: Date -> Props
defaultPropsFromDate today =
  defaultProps dayPickerProps
  where
    dayPickerProps = DayPicker.defaultProps today

-- | Default date format is `YYYY-M-D`.
defaultProps :: DayPicker.Props -> Props
defaultProps dayPickerProps =
  { dayPickerProps: dayPickerProps
  , value: Nothing
  , placeholder: "YYYY-M-D"
  , styles: defaultStyles
  , formatDate: defaultFormatDate
  , parseDate: defaultParseDate
  }

type State =
  { dayPickerProps :: DayPicker.Props
  , focused :: Boolean
  , value :: String
  , placeholder :: String
  , styles :: Styles
  , formatDate :: Date -> String
  , parseDate :: String -> Maybe Date
  }

initialState :: Props -> State
initialState { dayPickerProps, value, placeholder, styles, formatDate, parseDate } =
  { dayPickerProps
  , focused: false
  , value: maybe "" formatDate value
  , placeholder
  , styles
  , formatDate
  , parseDate
  }

updateStateWithProps :: Props -> State -> State
updateStateWithProps { dayPickerProps, value, placeholder, styles, formatDate, parseDate } state =
  state { dayPickerProps = dayPickerProps
   , value = maybe state.value formatDate value
   , placeholder = placeholder
   , styles = styles
   , formatDate = formatDate
   , parseDate = parseDate
   }

-- Handle focus and `onValueInput`.
data Query a
  = Init a
  | ClickDocument Event a
  | OnReceiveProps Props a
  | HandleDayPicker DayPicker.Message a
  | Focus a
  | OnFocus a
  | OnInput String a

-- It wraps `DayPicker.Message` as `Select Date`. Input value change is first parsed as `Maybe Date`, then sent to parent component.
data Message
  = Select Date
  | Input (Maybe Date)

type Slot = Unit

rootRef :: H.RefLabel
rootRef = H.RefLabel "root"

inputRef :: H.RefLabel
inputRef = H.RefLabel "input"

render
  :: forall eff m
  .  MonadAff (HalogenEffects eff) m
  => State -> H.ParentHTML Query DayPicker.Query Unit m
render state@{ styles, value } =
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
        , HE.onValueInput $ HE.input OnInput
        ]
    , if state.focused
      then HH.div [ HP.class_ styles.dropdown ] [ dayPicker ]
      else HH.text ""
    ]
  where
    dayPicker =
      HH.slot unit DayPicker.dayPicker state.dayPickerProps (HE.input HandleDayPicker)

-- | A components that includes a text input and `DayPicker.dayPicker`.
dayPickerInput
  :: forall eff m
  .  MonadAff (HalogenEffects eff) m
  => H.Component HH.HTML Query Props Message m
dayPickerInput = H.lifecycleParentComponent
  { initialState
  , render
  , eval
  , receiver: HE.input OnReceiveProps
  , initializer: Just $ H.action Init
  , finalizer: Nothing
  }
  where

  eval :: Query ~> H.ParentDSL State Query DayPicker.Query Slot Message m
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

  eval (OnInput value next) = do
    H.gets _.parseDate >>= \parseDate -> H.raise $ Input $ parseDate value
    pure next

  eval (OnReceiveProps input next) = do
    H.modify $ updateStateWithProps input
    pure next

  eval (HandleDayPicker date next) = do
    H.modify $ _{ focused = false }
    H.raise $ Select date
    pure next
