module Halogen.DayPicker where

import Prelude

import Data.Array ((..), elemIndex, replicate)
import Data.Date (Date, Weekday(..), Day)
import Data.Date as Date
import Data.DateTime as DateTime
import Data.Time.Duration as Duration
import Data.Enum (fromEnum, toEnum)
import Data.Maybe (Maybe(..), fromMaybe, maybe)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

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

data Query a
  = Click Date a
  | PrevMonth a
  | NextMonth a

type State =
  { today :: Date
  , firstDateOfFirstMonth :: Date
  }

type Input = Date

type Message = Date

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

renderTableHeader :: H.ComponentHTML Query
renderTableHeader =
  HH.thead_
    [ HH.tr_ (map render' weekdays)
    ]
  where
    render' day = HH.td_ [ HH.text $ pprWeekday day ]

renderDay :: Date -> Maybe Day -> H.ComponentHTML Query
renderDay _ Nothing = HH.td_ [ HH.text "" ]
renderDay firstDate (Just day) =
  HH.td
    [ HE.onClick $ HE.input (const $ Click clickedDay) ]
    [ HH.text $ show $ fromEnum day ]
  where
    year = Date.year firstDate
    month = Date.month firstDate
    clickedDay = Date.canonicalDate year month day

renderDayRow :: Date -> Int -> Day -> Int -> H.ComponentHTML Query
renderDayRow firstDate firstDayColIndex lastDay rowIndex =
  HH.tr_ $
    replicate startCol (renderDay firstDate Nothing) <>
    map (renderDay firstDate <<< toEnum) (startDayInt .. endDayInt)
  where
    startCol = if rowIndex == 0 then firstDayColIndex else 0
    startDayInt = if rowIndex == 0 then 1 else 7 * rowIndex - firstDayColIndex + 1
    endDayInt = if startDayInt + 6 < fromEnum lastDay
                then startDayInt + 6 - startCol else fromEnum lastDay

renderTableBody :: State -> H.ComponentHTML Query
renderTableBody state =
  HH.tbody_ $
    map (renderDayRow firstDate firstDayColIndex lastDay) (0 .. lastRowIndex)
  where
    firstDate = state.firstDateOfFirstMonth
    weekdayOfFirstDay = Date.weekday firstDate
    firstDayColIndex = fromMaybe 0 $ elemIndex weekdayOfFirstDay weekdays
    lastDay = Date.day $ lastDateOfMonth firstDate
    lastRowIndex = (firstDayColIndex + fromEnum lastDay - 1) / 7

render :: State -> H.ComponentHTML Query
render state =
  HH.div_
    [ HH.div_
        [ HH.button
            [ HE.onClick (HE.input_ PrevMonth) ] [ HH.text "prev" ]
        , HH.text headText
        , HH.button
            [ HE.onClick (HE.input_ NextMonth) ] [ HH.text "next" ]
        ]
    , HH.table_
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
    , receiver: const Nothing
    }
  where

  initialState :: Input -> State
  initialState today =
    let year = Date.year today
        month = Date.month today
        firstDateOfFirstMonth = Date.canonicalDate year month bottom
     in
        { today: today
        , firstDateOfFirstMonth: firstDateOfFirstMonth
        }

  eval :: Query ~> H.ComponentDSL State Query Message m
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
