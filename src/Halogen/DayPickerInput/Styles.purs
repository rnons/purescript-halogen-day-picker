module Halogen.DayPickerInput.Styles
  ( Styles
  , defaultStyles
  ) where

import Halogen.HTML (ClassName(ClassName))

type Styles =
  { dayPickerInput :: ClassName
  , dayPickerInputInput :: ClassName
  , dayPickerInputDropdown :: ClassName
  }

defaultStyles :: Styles
defaultStyles =
  { dayPickerInput: ClassName "DayPickerInput"
  , dayPickerInputInput: ClassName "DayPickerInput-input"
  , dayPickerInputDropdown: ClassName "DayPickerInput-dropdown"
  }
