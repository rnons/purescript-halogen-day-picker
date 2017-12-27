module Halogen.DayPickerInput.Styles
  ( Styles
  , defaultStyles
  ) where

import Halogen.HTML (ClassName(ClassName))

type Styles =
  { root :: ClassName
  , input :: ClassName
  , dropdown :: ClassName
  }

defaultStyles :: Styles
defaultStyles =
  { root: ClassName "DayPickerInput"
  , input: ClassName "DayPickerInput-input"
  , dropdown: ClassName "DayPickerInput-dropdown"
  }
