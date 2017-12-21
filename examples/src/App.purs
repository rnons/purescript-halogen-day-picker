module App where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Date (Date)

import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)

import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Halogen.DayPicker as DayPicker

import Route (Route(..))
import Examples.Simple as ExpSimple
import Examples.SimpleInput as ExpSimpleInput

data Query a
  = DayPickerChange DayPicker.Message a
  | RouteChange Route a

type State =
  { selectedDate :: Maybe Date
  , route :: Route
  }

type ChildQuery = Coproduct2 ExpSimple.Query ExpSimpleInput.Query

type Slot = Either2 Unit Unit

renderMain :: forall m. Route -> Date -> H.ParentHTML Query ChildQuery Slot m
renderMain Simple today =
  HH.slot' CP.cp1 unit (ExpSimple.component today) unit absurd
renderMain SimpleInput today =
  HH.slot' CP.cp2 unit (ExpSimpleInput.component today) unit absurd
renderMain _ today =
  HH.text "main body"

app :: forall m. Date -> H.Component HH.HTML Query Unit Void m
app today =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { selectedDate: Nothing, route: Home }

  render :: State -> H.ParentHTML Query ChildQuery Slot m
  render state =
    HH.div [ HP.class_ $ HH.ClassName "container" ]
      [ HH.div [ HP.class_ $ HH.ClassName "sidebar" ]
          [ HH.a [ HP.href $ show Home ]
              [ HH.text "Home" ]
          , HH.ul_
              [ HH.li_
                  [ HH.a [ HP.href $ show Simple ]
                      [ HH.text "Simple day picker" ]
                  ]
              , HH.li_
                  [ HH.a [ HP.href $ show SimpleInput ]
                      [ HH.text "Simple day picker input" ]
                  ]
              ]
          ]
      , HH.div [ HP.class_ $ HH.ClassName "main" ]
          [ renderMain state.route today ]
      ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery Slot Void m
  eval = case _ of
    DayPickerChange date next -> do
      H.modify (\state -> state { selectedDate = Just date })
      pure next
    RouteChange route next -> do
      H.modify (\state -> state { route = route })
      pure next
