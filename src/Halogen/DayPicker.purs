module Halogen.DayPicker where

import Prelude

import Data.Array ((..), elemIndex, foldl, mapWithIndex, replicate)
import Data.Date (Date, Weekday(..), Day)
import Data.Date as Date
import Data.DateTime as DateTime
import Data.Enum (fromEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Time.Duration as Duration

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Halogen.DayPicker.Styles (Styles, defaultStyles)

weekdays :: Array Weekday
weekdays = [ Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday ]

pprWeekday :: Weekday -> String
pprWeekday Monday = "月"
pprWeekday Tuesday = "火"
pprWeekday Wednesday = "水"
pprWeekday Thursday = "木"
pprWeekday Friday = "金"
pprWeekday Saturday = "土"
pprWeekday Sunday = "日"

-- TODO: support `Set Date`
data SelectedDate
  = NoneSelected
  | Single Date
  | FromTo (Maybe Date) (Maybe Date)

-- TODO: support `Set Date` and `Date -> Boolean`
data DisabledDate
  = NoneDisabled
  | Before Date
  | After Date

derive instance genericRepSelectedDate :: Generic SelectedDate _
instance showSelectedDate :: Show SelectedDate where show = genericShow

type Props =
  { today :: Date
  , selectedDate :: SelectedDate
  , disabledDate :: DisabledDate
  , numberOfMonths :: Int
  , styles :: Styles
  }

type State =
  { today :: Date
  , firstDateOfFirstMonth :: Date
  , selectedDate :: SelectedDate
  , disabledDate :: DisabledDate
  , numberOfMonths :: Int
  , styles :: Styles
  }

data Query a
  = OnReceiveProps Props a
  | Click Date a
  | PrevMonth a
  | NextMonth a

type Message = Date

applyN :: forall a. Int -> (a -> a) -> a -> a
applyN n fn val =
    foldl (\acc _ -> fn acc) val (replicate n unit)

isDateSelected :: SelectedDate -> Date -> Boolean
isDateSelected NoneSelected _ = false
isDateSelected (Single d) date = d == date
isDateSelected (FromTo (Just from) (Just to)) date = from <= date && date <= to
isDateSelected (FromTo (Just from) _) date = from == date
isDateSelected (FromTo _ _) date = false

isDateDisabled :: DisabledDate -> Date -> Boolean
isDateDisabled NoneDisabled _ = false
isDateDisabled (Before d) date = date < d
isDateDisabled (After d) date = date > d

defaultProps :: Date -> Props
defaultProps today =
  { today: today
  , selectedDate: NoneSelected
  , disabledDate: NoneDisabled
  , numberOfMonths: 1
  , styles: defaultStyles
  }

updateStateWithProps :: Props -> State -> State
updateStateWithProps { today, selectedDate, disabledDate, numberOfMonths, styles } =
  _{ today = today
   , selectedDate = selectedDate
   , disabledDate = disabledDate
   , numberOfMonths = numberOfMonths
   , firstDateOfFirstMonth = getFirstDateOfFirstMonth selectedDate today
   , styles = styles
   }

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

getFirstDateOfFirstMonth :: SelectedDate -> Date -> Date
getFirstDateOfFirstMonth (Single date) _ = firstDateOfMonth date
getFirstDateOfFirstMonth (FromTo (Just date) _) _ = firstDateOfMonth date
getFirstDateOfFirstMonth _ today = firstDateOfMonth today

renderTableHeader :: Styles -> H.ComponentHTML Query
renderTableHeader styles =
  HH.thead_
    [ HH.tr_ (map render' weekdays)
    ]
  where
    render' day =
      HH.td [ HP.class_ styles.weekday ] [ HH.text $ pprWeekday day ]

renderDay :: State -> Date -> Maybe Day -> H.ComponentHTML Query
renderDay _ _ Nothing = HH.td_ [ HH.text "" ]
renderDay state@{ styles } firstDate (Just day) =
  HH.td
    props
    [ HH.text $ show $ fromEnum day
    ]
  where
    year = Date.year firstDate
    month = Date.month firstDate
    date = Date.canonicalDate year month day
    isDisabled = isDateDisabled state.disabledDate date
    className =
      if isDisabled then styles.dayDisabled else
        if isDateSelected state.selectedDate date
        then styles.daySelected else styles.day
    props =
      if isDisabled
      then [ HP.class_ className ]
      else [ HP.class_ className, HE.onClick $ HE.input (const $ Click date) ]

renderDayRow :: State -> Date -> Int -> Day -> Int -> H.ComponentHTML Query
renderDayRow state firstDate firstDayColIndex lastDay rowIndex =
  HH.tr_ $
    replicate startCol (renderDay state firstDate Nothing) <>
    map (renderDay state firstDate <<< toEnum) (startDayInt .. endDayInt)
  where
    startCol = if rowIndex == 0 then firstDayColIndex else 0
    startDayInt = if rowIndex == 0 then 1 else 7 * rowIndex - firstDayColIndex + 1
    endDayInt = if startDayInt + 6 < fromEnum lastDay
                then startDayInt + 6 - startCol else fromEnum lastDay

renderTableBody :: State -> Date -> H.ComponentHTML Query
renderTableBody state firstDate =
  HH.tbody_ $
    map (renderDayRow state firstDate firstDayColIndex lastDay) (0 .. lastRowIndex)
  where
    weekdayOfFirstDay = Date.weekday firstDate
    firstDayColIndex = fromMaybe 0 $ elemIndex weekdayOfFirstDay weekdays
    lastDay = Date.day $ lastDateOfMonth firstDate
    lastRowIndex = (firstDayColIndex + fromEnum lastDay - 1) / 7

renderMonth :: State -> Int -> H.ComponentHTML Query
renderMonth state@{ styles } index =
  HH.div
    [ HP.class_ styles.month ]
    [ HH.div
        [ HP.class_ styles.head ]
        [ HH.button
            [ HP.classes
                [ styles.control
                , if showPrev then styles.controlPrev else styles.controlHidden
                ]
            , HE.onClick (HE.input_ PrevMonth)
            ]
            []
        , HH.text headText
        , HH.button
            [ HP.classes
                [ styles.control
                , if showNext then styles.controlNext else styles.controlHidden
                ]
            , HE.onClick (HE.input_ NextMonth)
            ]
            []
        ]
    , HH.table
        [ HP.class_ styles.body ]
        [ renderTableHeader styles
        , renderTableBody state firstDate
        ]
    ]
  where
    firstDate = applyN index firstDateOfNextMonth state.firstDateOfFirstMonth
    yearStr = show $ fromEnum $ Date.year firstDate
    monthStr = show $ fromEnum $ Date.month firstDate
    headText = yearStr <> "年" <> monthStr <> "月"
    showPrev = index == 0
    showNext = index + 1 == state.numberOfMonths

render :: State -> H.ComponentHTML Query
render state@{ styles } =
  HH.div
    [ HP.class_ styles.root ]
    $ mapWithIndex (\index _ -> renderMonth state index) (1..state.numberOfMonths)

dayPicker :: forall m. H.Component HH.HTML Query Props Message m
dayPicker =
  H.component
    { initialState: initialState
    , render
    , eval
    , receiver: HE.input OnReceiveProps
    }
  where

  initialState :: Props -> State
  initialState { today, selectedDate, disabledDate, numberOfMonths, styles } =
    { today: today
    , firstDateOfFirstMonth: getFirstDateOfFirstMonth selectedDate today
    , selectedDate: selectedDate
    , disabledDate: disabledDate
    , numberOfMonths: numberOfMonths
    , styles: styles
    }

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval (OnReceiveProps input next) = do
    H.modify $ updateStateWithProps input
    pure next
  eval (Click date next) = do
    H.raise date
    pure next
  eval (PrevMonth next) = do
    H.modify $ \state ->
      state { firstDateOfFirstMonth = firstDateOfPrevMonth state.firstDateOfFirstMonth }
    pure next
  eval (NextMonth next) = do
    H.modify $ \state ->
      state { firstDateOfFirstMonth = firstDateOfNextMonth state.firstDateOfFirstMonth }
    pure next
