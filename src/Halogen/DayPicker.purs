-- | A calendar component that can be customized by `Props`. It is a controlled
-- | component, which means it has minimum internal state and is controlled by
-- | parent component.
module Halogen.DayPicker
  ( SelectedDate(..)
  , DisabledDate(..)
  , Mode(..)
  , Props
  , Query(..)
  , Message
  , defaultProps
  , dayPicker
  , module Halogen.DayPicker.Styles
  ) where

import Prelude

import Data.Array ((..), elemIndex, foldl, mapWithIndex, replicate)
import Data.Date (Date, Weekday(..), Day)
import Data.Date as Date
import Data.DateTime as DateTime
import Data.Enum (fromEnum, toEnum)
import Data.Foldable (any)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Time.Duration as Duration
import Halogen as H
import Halogen.DayPicker.Styles (Styles, defaultStyles)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

weekdays :: Array Weekday
weekdays = [ Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday ]

defaultFormatMonth :: Date -> String
defaultFormatMonth date = monthStr <> " " <> yearStr
  where
    yearStr = show $ fromEnum $ Date.year date
    monthStr = show $ Date.month date

defaultFormatWeekday :: Weekday -> String
defaultFormatWeekday Monday = "M"
defaultFormatWeekday Tuesday = "T"
defaultFormatWeekday Wednesday = "W"
defaultFormatWeekday Thursday = "T"
defaultFormatWeekday Friday = "F"
defaultFormatWeekday Saturday = "S"
defaultFormatWeekday Sunday = "S"

-- | `SelectedDate` can be a single `Date` or a range of `Date`s.
data SelectedDate
  = SelectedNone
  | SelectedSingle Date
  | SelectedSet (Set Date)
  | SelectedRange (Maybe Date) (Maybe Date)

derive instance genericRepSelectedDate :: Generic SelectedDate _
instance showSelectedDate :: Show SelectedDate where show = genericShow
derive instance eqSelectedDate :: Eq SelectedDate

-- | Disable dates `DisabledBefore` or `DisabledAfter` a specific `Date`, can
-- | also be a combination of them.
-- TODO: support `Set Date` and `Date -> Boolean`
data DisabledDate
  = DisabledNone
  | DisabledBefore Date
  | DisabledAfter Date
  | DisabledArray (Array DisabledDate)

data Mode
  = SimpleMode
  | ToMode

-- | Month title, weekday text, class name can be customized. Note that `today`
-- | is not an internal state, but passed in.
type Props =
  { mode :: Mode
  , today :: Date
  , selectedDate :: SelectedDate
  , disabledDate :: DisabledDate
  , numberOfMonths :: Int
  , styles :: Styles
  , formatMonth :: Date -> String
  , formatWeekday :: Weekday -> String
  }

-- | Construct default props from `today` date.
defaultProps :: Date -> Props
defaultProps today =
  { mode: SimpleMode
  , today: today
  , selectedDate: SelectedSet Set.empty
  , disabledDate: DisabledNone
  , numberOfMonths: 1
  , styles: defaultStyles
  , formatMonth: defaultFormatMonth
  , formatWeekday: defaultFormatWeekday
  }

-- | The only interal state is `firstDateOfFirstMonth`, which is the first date
-- | of currently rendered months. Other states are all passed in.
type State =
  { props :: Props
  , firstDateOfFirstMonth :: Date
  }

initialState :: Props -> State
initialState props =
  { props
  , firstDateOfFirstMonth: getFirstDateOfFirstMonth props
  }

updateStateWithProps :: Props -> State -> State
updateStateWithProps props state = state
  { props = props
  }

-- | The behavior of this component includes selecting date and navigating months.
data Query a
  = OnReceiveProps Props a
  | Click Date a
  | PrevMonth a
  | NextMonth a

-- | This component raises selected `Date` to be handled by parent component.
type Message = Date

type HTML m = H.ComponentHTML Query () m

applyN :: forall a. Int -> (a -> a) -> a -> a
applyN n fn val =
    foldl (\acc _ -> fn acc) val (replicate n unit)

isDateSelected :: SelectedDate -> Date -> Boolean
isDateSelected SelectedNone _ = false
isDateSelected (SelectedSingle d) date = d == date
isDateSelected (SelectedSet s) date = Set.member date s
isDateSelected (SelectedRange (Just from) (Just to)) date = from <= date && date <= to
isDateSelected (SelectedRange (Just from) _) date = from == date
isDateSelected (SelectedRange _ _) date = false

isDateFrom :: SelectedDate -> Date -> Boolean
isDateFrom (SelectedRange (Just from) _) date = from == date
isDateFrom _ _ = false

isDateTo :: SelectedDate -> Date -> Boolean
isDateTo (SelectedRange _ (Just to)) date = to == date
isDateTo _ _ = false

isDateDisabled :: DisabledDate -> Date -> Boolean
isDateDisabled DisabledNone _ = false
isDateDisabled (DisabledBefore d) date = date < d
isDateDisabled (DisabledAfter d) date = date > d
isDateDisabled (DisabledArray rules) date = any (flip isDateDisabled $ date) rules

firstDateOfMonth :: Date -> Date
firstDateOfMonth date =
  Date.canonicalDate year month bottom
  where
    year = Date.year date
    month = Date.month date

lastDateOfMonth :: Date -> Date
lastDateOfMonth date =
  Date.canonicalDate year month lastDay
  where
    year = Date.year date
    month = Date.month date
    lastDay = Date.lastDayOfMonth year month

firstDateOfPrevMonth :: Date -> Date
firstDateOfPrevMonth date =
  maybe date (firstDateOfMonth <<< DateTime.date) newDateTime
  where
    firstDate = firstDateOfMonth date
    dateTime = DateTime.DateTime firstDate bottom
    newDateTime = DateTime.adjust (Duration.Days (-1.0)) dateTime

firstDateOfNextMonth :: Date -> Date
firstDateOfNextMonth date =
  maybe date DateTime.date newDateTime
  where
    lastDate = lastDateOfMonth date
    dateTime = DateTime.DateTime lastDate bottom
    newDateTime = DateTime.adjust (Duration.Days 1.0) dateTime

getFirstDateOfFirstMonth :: Props -> Date
getFirstDateOfFirstMonth { selectedDate, mode, today, numberOfMonths } =
  case selectedDate of
    SelectedNone -> firstDateOfMonth today
    SelectedSingle date -> firstDateOfMonth date
    SelectedSet set -> firstDateOfMonth $ fromMaybe today $ Set.findMin set
    SelectedRange startDate endDate ->
      case mode, startDate, endDate of
        SimpleMode, (Just date), _ -> firstDateOfMonth date
        ToMode, _, (Just date) ->
          applyN (numberOfMonths - 1) firstDateOfPrevMonth (firstDateOfMonth date)
        _, _, _ -> firstDateOfMonth today

renderTableHeader :: forall m. State -> Styles -> HTML m
renderTableHeader { props } styles =
  HH.thead_
  [ HH.tr_ (map render' weekdays)
  ]
  where
  render' day =
    HH.td [ HP.class_ styles.weekday ] [ HH.text $ props.formatWeekday day ]

renderDay :: forall m. State -> Date -> Maybe Day -> HTML m
renderDay _ _ Nothing = HH.td_ [ HH.text "" ]
renderDay state@{ props: { styles, selectedDate, today } } firstDate (Just day) =
  HH.td
    props
    [ HH.text $ show $ fromEnum day
    ]
  where
    year = Date.year firstDate
    month = Date.month firstDate
    date = Date.canonicalDate year month day
    todayCls = if today == date then [ styles.dayToday ] else []
    isDisabled = isDateDisabled state.props.disabledDate date
    disabled =
      if isDisabled then [ styles.dayIsDisabled ] else []
    selected =
      if isDateSelected selectedDate date then [ styles.dayIsSelected ] else []
    from =
      if isDateFrom selectedDate date then [ styles.dayFrom ] else []
    to =
      if isDateTo selectedDate date then [ styles.dayTo ] else []
    className =
      [ styles.day ] <> todayCls <> from <> to <> selected <> disabled
    props =
      if isDisabled
      then [ HP.classes className ]
      else [ HP.classes className, HE.onClick $ HE.input (const $ Click date) ]

renderDayRow :: forall m. State -> Date -> Int -> Day -> Int -> HTML m
renderDayRow state firstDate firstDayColIndex lastDay rowIndex =
  HH.tr_ $
    replicate startCol (renderDay state firstDate Nothing) <>
    map (renderDay state firstDate <<< toEnum) (startDayInt .. endDayInt)
  where
    startCol = if rowIndex == 0 then firstDayColIndex else 0
    startDayInt = if rowIndex == 0 then 1 else 7 * rowIndex - firstDayColIndex + 1
    endDayInt = if startDayInt + 6 < fromEnum lastDay
                then startDayInt + 6 - startCol else fromEnum lastDay

renderTableBody :: forall m. State -> Date -> HTML m
renderTableBody state firstDate =
  HH.tbody_ $
    map (renderDayRow state firstDate firstDayColIndex lastDay) (0 .. lastRowIndex)
  where
    weekdayOfFirstDay = Date.weekday firstDate
    firstDayColIndex = fromMaybe 0 $ elemIndex weekdayOfFirstDay weekdays
    lastDay = Date.day $ lastDateOfMonth firstDate
    lastRowIndex = (firstDayColIndex + fromEnum lastDay - 1) / 7

renderMonth :: forall m. State -> Int -> HTML m
renderMonth state@{ props: { styles, formatMonth } } index =
  HH.div
    [ HP.class_ styles.month ]
    [ HH.div
        [ HP.class_ styles.head ]
        [ HH.button
            [ HP.classes
                [ styles.control
                , if showPrev then styles.controlPrev else styles.controlIsHidden
                ]
            , HP.type_ HP.ButtonButton
            , HE.onClick (HE.input_ PrevMonth)
            ]
            []
        , HH.text headText
        , HH.button
            [ HP.classes
                [ styles.control
                , if showNext then styles.controlNext else styles.controlIsHidden
                ]
            , HP.type_ HP.ButtonButton
            , HE.onClick (HE.input_ NextMonth)
            ]
            []
        ]
    , HH.table
        [ HP.class_ styles.body ]
        [ renderTableHeader state styles
        , renderTableBody state firstDate
        ]
    ]
  where
  firstDate = applyN index firstDateOfNextMonth state.firstDateOfFirstMonth
  headText = formatMonth firstDate
  showPrev = index == 0
  showNext = index + 1 == state.props.numberOfMonths

render :: forall m. State -> HTML m
render state@{ props } =
  HH.div
    [ HP.class_ props.styles.root ]
    $ mapWithIndex (\index _ -> renderMonth state index) (1..props.numberOfMonths)

-- | A simple calendar that raises selected date.
dayPicker :: forall m. H.Component HH.HTML Query Props Message m
dayPicker = H.component
  { initialState
  , render
  , eval
  , receiver: HE.input OnReceiveProps
  , initializer: Nothing
  , finalizer: Nothing
  }
  where
  eval :: Query ~> H.HalogenM State Query () Message m
  eval (OnReceiveProps input n) = n <$ do
    H.modify_ $ updateStateWithProps input

  eval (Click date n) = n <$ do
    H.raise date

  eval (PrevMonth n) = n <$ do
    H.modify_ $ \state ->
      state { firstDateOfFirstMonth = firstDateOfPrevMonth state.firstDateOfFirstMonth }

  eval (NextMonth n) = n <$ do
    H.modify_ $ \state ->
      state { firstDateOfFirstMonth = firstDateOfNextMonth state.firstDateOfFirstMonth }
