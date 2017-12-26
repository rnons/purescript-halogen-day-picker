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

data Query a
  = HandleInput Input a
  | Click Date a
  | PrevMonth a
  | NextMonth a

type State =
  { today :: Date
  , firstDateOfFirstMonth :: Date
  , selectedDate :: SelectedDate
  , disabledDate :: DisabledDate
  , numberOfMonths :: Int
  }

type Input =
  { today :: Date
  , selectedDate :: SelectedDate
  , disabledDate :: DisabledDate
  , numberOfMonths :: Int
  }

type Message = Date

applyN :: forall a. Int -> (a -> a) -> a -> a
applyN n fn val =
    foldl (\acc _ -> fn acc) val (replicate n unit)

isDateSelected :: SelectedDate -> Date -> Boolean
isDateSelected NoneSelected _ = false
isDateSelected (Single d) date = d == date
isDateSelected (FromTo (Just from) (Just to)) date = from <= date && date <= to
isDateSelected (FromTo _ _) date = false

isDateDisabled :: DisabledDate -> Date -> Boolean
isDateDisabled NoneDisabled _ = false
isDateDisabled (Before d) date = date < d
isDateDisabled (After d) date = date > d

defaultInput :: Date -> Input
defaultInput today =
  { today: today
  , selectedDate: NoneSelected
  , disabledDate: NoneDisabled
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

classes :: forall r i. Array String -> HP.IProp ("class" :: String | r) i
classes = HP.classes <<< map HH.ClassName

renderTableHeader :: H.ComponentHTML Query
renderTableHeader =
  HH.thead_
    [ HH.tr_ (map render' weekdays)
    ]
  where
    render' day =
      HH.td [ class_ "DayPicker-weekday" ] [ HH.text $ pprWeekday day ]

renderDay :: State -> Date -> Maybe Day -> H.ComponentHTML Query
renderDay _ _ Nothing = HH.td_ [ HH.text "" ]
renderDay state firstDate (Just day) =
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
      if isDisabled then "DayPicker-day is-disabled" else
        if isDateSelected state.selectedDate date
        then "DayPicker-day is-selected" else "DayPicker-day"
    props =
      if isDisabled
      then [ class_ className ]
      else [ class_ className, HE.onClick $ HE.input (const $ Click date) ]

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
renderMonth state index =
  HH.div
    [ class_ "DayPicker-month" ]
    [ HH.div
        [ class_ "DayPicker-head" ]
        [ HH.button
            [ classes
                [ "DayPicker-control"
                , if showPrev then "DayPicker-control--prev" else "is-hidden"
                ]
            , HE.onClick (HE.input_ PrevMonth)
            ]
            []
        , HH.text headText
        , HH.button
            [ classes
                [ "DayPicker-control"
                , if showNext then "DayPicker-control--next" else "is-hidden"
                ]
            , HE.onClick (HE.input_ NextMonth)
            ]
            []
        ]
    , HH.table
        [ class_ "DayPicker-body" ]
        [ renderTableHeader
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
render state =
  HH.div
    [ class_ "DayPicker" ]
    $ mapWithIndex (\index _ -> renderMonth state index) (1..state.numberOfMonths)

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
  initialState { today, selectedDate, disabledDate, numberOfMonths } =
    { today: today
    , firstDateOfFirstMonth: firstDateOfFirstMonth
    , selectedDate: selectedDate
    , disabledDate: disabledDate
    , numberOfMonths: numberOfMonths
    }
    where
    year = Date.year today
    month = Date.month today
    firstDateOfFirstMonth = Date.canonicalDate year month bottom

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
