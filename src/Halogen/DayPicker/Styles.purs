module Halogen.DayPicker.Styles
  ( Styles
  , defaultStyles
  ) where

import Halogen.HTML (ClassName(ClassName))

type Styles =
  { root :: ClassName
  , month :: ClassName
  , head :: ClassName
  , control :: ClassName
  , controlPrev :: ClassName
  , controlNext :: ClassName
  , controlIsHidden :: ClassName
  , body :: ClassName
  , weekday :: ClassName
  , day :: ClassName
  , dayIsSelected :: ClassName
  , dayIsDisabled :: ClassName
  }

defaultStyles :: Styles
defaultStyles =
  { root: ClassName "DayPicker"
  , month: ClassName "DayPicker-month"
  , head: ClassName "DayPicker-head"
  , control: ClassName "DayPicker-control"
  , controlPrev: ClassName "DayPicker-control--prev"
  , controlNext: ClassName "DayPicker-control--next"
  , controlIsHidden: ClassName "is-hidden"
  , body: ClassName "DayPicker-body"
  , weekday: ClassName "DayPicker-weekday"
  , day: ClassName "DayPicker-day"
  , dayIsSelected: ClassName "is-selected"
  , dayIsDisabled: ClassName "is-disabled"
  }
