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

import DOM.HTML.Indexed (HTMLinput)
import Data.Date (Date)
import Data.Date as Date
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(Pattern), split)
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sequence)
import Effect.Aff (delay, forkAff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.DayPicker as DayPicker
import Halogen.DayPickerInput.Styles (Styles, defaultStyles)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.HTML.HTMLElement (focus)

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
  , inputProps :: Array (HH.IProp HTMLinput (Query Unit))
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
  , inputProps: []
  , value: Nothing
  , placeholder: "YYYY-M-D"
  , styles: defaultStyles
  , formatDate: defaultFormatDate
  , parseDate: defaultParseDate
  }

type State =
  { dayPickerProps :: DayPicker.Props
  , inputProps :: Array (HH.IProp HTMLinput (Query Unit))
  , value :: String
  , placeholder :: String
  , styles :: Styles
  , formatDate :: Date -> String
  , parseDate :: String -> Maybe Date
  , focused :: Boolean
  , clickedInside :: Boolean
  }

initialState :: Props -> State
initialState { dayPickerProps, inputProps, value, placeholder, styles, formatDate, parseDate } =
  { dayPickerProps
  , inputProps
  , value: maybe "" formatDate value
  , placeholder
  , styles
  , formatDate
  , parseDate
  , focused: false
  , clickedInside: false
  }

updateStateWithProps :: Props -> State -> State
updateStateWithProps { dayPickerProps, inputProps, value, placeholder, styles, formatDate, parseDate } state = state
  { dayPickerProps = dayPickerProps
  , inputProps = inputProps
  , value = maybe state.value formatDate value
  , placeholder = placeholder
  , styles = styles
  , formatDate = formatDate
  , parseDate = parseDate
  }

-- Handle focus and `onValueInput`.
data Query a
  = OnReceiveProps Props a
  | HandleDayPicker DayPicker.Message a
  | Focus a
  | OnMouseDown a
  | OnMouseUp a
  | OnFocus a
  | OnBlur a
  | OnInput String a

-- It wraps `DayPicker.Message` as `Select Date`. Input value change is first parsed as `Maybe Date`, then sent to parent component.
data Message
  = Select Date
  | Input (Maybe Date)

type Slot = (picker :: H.Slot DayPicker.Query DayPicker.Message Unit)

type HTML m = H.ComponentHTML Query Slot m

inputRef :: H.RefLabel
inputRef = H.RefLabel "input"

render
  :: forall m
  .  MonadAff m
  => State -> HTML m
render state@{ styles, inputProps, value } =
  HH.div
    [ HP.class_ styles.root
    , HE.onMouseDown $ HE.input_ OnMouseDown
    , HE.onMouseUp $ HE.input_ OnMouseUp
    ]
    [ HH.input $ inputProps <>
        [ HP.type_ HP.InputText
        , HP.value value
        , HP.placeholder state.placeholder
        , HP.ref inputRef
        , HE.onFocus $ HE.input_ OnFocus
        , HE.onBlur $ HE.input_ OnBlur
        , HE.onValueInput $ HE.input OnInput
        ]
    , if state.focused
      then HH.div [ HP.class_ styles.dropdown ] [ dayPicker ]
      else HH.text ""
    ]
  where
    dayPicker =
      HH.slot (SProxy :: SProxy "picker") unit DayPicker.dayPicker state.dayPickerProps (HE.input HandleDayPicker)

-- | A components that includes a text input and `DayPicker.dayPicker`.
dayPickerInput
  :: forall m. MonadAff m => H.Component HH.HTML Query Props Message m
dayPickerInput = H.component
  { initialState
  , render
  , eval
  , receiver: HE.input OnReceiveProps
  , initializer: Nothing
  , finalizer: Nothing
  }
  where

  eval :: Query ~> H.HalogenM State Query Slot Message m
  eval (OnReceiveProps input n) = n <$ do
    H.modify_ $ updateStateWithProps input

  eval (HandleDayPicker date n) = n <$ do
    H.modify_ $ _{ focused = false }
    H.raise $ Select date

  eval (Focus n) = n <$ do
    mInput <- H.getHTMLElementRef inputRef
    case mInput of
      Just input -> do
        void $ H.liftAff $ forkAff $ do
          delay $ Milliseconds 10.0
          liftEffect $ focus input
      Nothing -> pure unit

  eval (OnMouseDown n) = n <$ do
    H.modify_ $ _{ clickedInside = true }

  eval (OnMouseUp n) = n <$ do
    H.modify_ $ _{ clickedInside = false }

  eval (OnFocus n) = n <$ do
    H.modify_ $ _{ focused = true }

  eval (OnBlur n) = n <$ do
    H.gets _.clickedInside >>= \clickedInside -> do
      H.modify_ $ _{ clickedInside = false }
      if clickedInside
        then eval (Focus unit)
        else do
          H.modify_ $ _{ focused = false }

  eval (OnInput value n) = n <$ do
    H.gets _.parseDate >>= \parseDate -> H.raise $ Input $ parseDate value
