module Halogen.DayPicker where

import Prelude

import Data.Array ((..), elemIndex, replicate)
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

data SelectedDate
  = None
  | Single Date

derive instance genericRepSelectedDate :: Generic SelectedDate _
instance showSelectedDate :: Show SelectedDate where show = genericShow

data Query a
  = HandleInput Input a
  | Click Date a
  | PrevMonth a
  | NextMonth a

type State =
  { today :: Date
  , firstDateOfFirstMonth :: Date
  , selectedDate :: SelectedDate
  }

type Input =
  { today :: Date
  , selectedDate :: SelectedDate
  , numberOfMonths :: Int
  }

type Message = Date

isDateSelected :: SelectedDate -> Date -> Boolean
isDateSelected None _ = false
isDateSelected (Single d) date = d == date

defaultInput :: Date -> Input
defaultInput today =
  { today: today
  , selectedDate: None
  , numberOfMonths: 1
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

class_ :: forall r i. String -> HP.IProp ("class" :: String | r) i
class_ = HP.class_ <<< HH.ClassName

renderTableHeader :: H.ComponentHTML Query
renderTableHeader =
  HH.thead_
    [ HH.tr_ (map render' weekdays)
    ]
  where
    render' day = HH.td_ [ HH.text $ pprWeekday day ]

renderDay :: State -> Date -> Maybe Day -> H.ComponentHTML Query
renderDay _ _ Nothing = HH.td_ [ HH.text "" ]
renderDay state firstDate (Just day) =
  HH.td
    [ class_ className
    , HE.onClick $ HE.input (const $ Click date)
    ]
    [ HH.text $ show $ fromEnum day
    ]
  where
    year = Date.year firstDate
    month = Date.month firstDate
    date = Date.canonicalDate year month day
    className = if isDateSelected state.selectedDate date
                then "DayPicker-day is-selected" else "DayPicker-day"

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

renderTableBody :: State -> H.ComponentHTML Query
renderTableBody state =
  HH.tbody_ $
    map (renderDayRow state firstDate firstDayColIndex lastDay) (0 .. lastRowIndex)
  where
    firstDate = state.firstDateOfFirstMonth
    weekdayOfFirstDay = Date.weekday firstDate
    firstDayColIndex = fromMaybe 0 $ elemIndex weekdayOfFirstDay weekdays
    lastDay = Date.day $ lastDateOfMonth firstDate
    lastRowIndex = (firstDayColIndex + fromEnum lastDay - 1) / 7

render :: State -> H.ComponentHTML Query
render state =
  HH.div
    [ class_ "DayPicker" ]
    [ HH.div
        [ class_ "DayPicker-head" ]
        [ HH.button
            [ class_ "DayPicker-control DayPicker-control--prev"
            , HE.onClick (HE.input_ PrevMonth)
            ]
            []
        , HH.text headText
        , HH.button
            [ class_ "DayPicker-control DayPicker-control--next"
            , HE.onClick (HE.input_ NextMonth)
            ]
            []
        ]
    , HH.table
        [ class_ "DayPicker-body" ]
        [ renderTableHeader
        , renderTableBody state
        ]
    ]
  where
    yearStr = show $ fromEnum $ Date.year state.firstDateOfFirstMonth
    monthStr = show $ fromEnum $ Date.month state.firstDateOfFirstMonth
    headText = yearStr <> "年" <> monthStr <> "月"

dayPicker :: forall m. H.Component HH.HTML Query Input Message m
dayPicker =
  H.component
    { initialState: initialState
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  initialState :: Input -> State
  initialState { today, selectedDate } =
    let year = Date.year today
        month = Date.month today
        firstDateOfFirstMonth = Date.canonicalDate year month bottom
     in
        { today: today
        , firstDateOfFirstMonth: firstDateOfFirstMonth
        , selectedDate: selectedDate
        }

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval (HandleInput input next) = do
    H.modify $ _{ selectedDate = input.selectedDate }
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
