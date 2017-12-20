module Halogen.DayPicker where

import Prelude

import Data.Array ((..), elemIndex, replicate)
import Data.Bounded as Bounded
import Data.Date (Date, Weekday(..), Day)
import Data.Date as Date
import Data.Enum (fromEnum, toEnum)
import Data.Maybe (Maybe(..), fromMaybe)

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

data Query a = Click Date a

type State = { today :: Date }

type Input = Date

type Message = Date

renderTableHeader :: H.ComponentHTML Query
renderTableHeader =
  HH.thead_
    [ HH.tr_ (map render' weekdays)
    ]
  where
    render' day = HH.td_ [ HH.text $ pprWeekday day ]

renderDay :: Date -> Maybe Day -> H.ComponentHTML Query
renderDay _ Nothing = HH.td_ [ HH.text "" ]
renderDay firstDay (Just day) =
  HH.td
    [ HE.onClick $ HE.input (const $ Click clickedDay) ]
    [ HH.text $ show $ fromEnum day ]
  where
    year = Date.year firstDay
    month = Date.month firstDay
    clickedDay = Date.canonicalDate year month day

renderDayRow :: Date -> Int -> Day -> Int -> H.ComponentHTML Query
renderDayRow firstDay firstDayColIndex lastDay rowIndex =
  HH.tr_ $
    replicate startCol (renderDay firstDay Nothing) <>
    map (renderDay firstDay <<< toEnum) (startDayInt .. endDayInt)
  where
    startCol = if rowIndex == 0 then firstDayColIndex else 0
    startDayInt = if rowIndex == 0 then 1 else 7 * rowIndex - firstDayColIndex + 1
    endDayInt = if startDayInt + 6 < fromEnum lastDay
                then startDayInt + 6 - startCol else fromEnum lastDay

renderTableBody :: Date -> H.ComponentHTML Query
renderTableBody today =
  HH.tbody_ $
    map (renderDayRow firstDay firstDayColIndex lastDay) (0 .. lastRowIndex)
  where
    year = Date.year today
    month = Date.month today
    firstDay = Date.canonicalDate year month Bounded.bottom
    weekdayOfFirstDay = Date.weekday firstDay
    firstDayColIndex = fromMaybe 0 $ elemIndex weekdayOfFirstDay weekdays
    lastDay = Date.lastDayOfMonth year month
    lastRowIndex = (fromEnum lastDay) / 7 + 1

renderCalendar :: Date -> H.ComponentHTML Query
renderCalendar today =
  HH.div_
    [ HH.p_
        [ HH.text $ show today ]
    , HH.table_
        [ renderTableHeader
        , renderTableBody today
        ]
    ]

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
  initialState input = { today: input }

  render :: State -> H.ComponentHTML Query
  render state =
    renderCalendar state.today

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Click date next -> do
      H.raise date
      pure next
