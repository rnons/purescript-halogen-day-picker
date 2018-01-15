module App where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Date (Date)

import Data.Const (Const)

import Halogen as H
import Halogen.Component.Proxy (ProxyQ, proxy)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Route (Route(..))
import Examples.Types (AppM)
import Examples.Simple as ExpSimple
import Examples.SimpleInput as ExpSimpleInput
import Examples.RangeWithTwoInputs as ExpRangeInputs

data Query a
  = RouteChange Route a

type State =
  { selectedDate :: Maybe Date
  , route :: Route
  }

type ChildQuery = ProxyQ (Const Void) Unit Void

type Slot = Int

renderMain :: Route -> Date -> H.ParentHTML Query ChildQuery Slot AppM
renderMain Simple today =
  HH.slot 1 (proxy $ ExpSimple.component today) unit absurd
renderMain SimpleInput today =
  HH.slot 2 (proxy $ ExpSimpleInput.component today) unit absurd
renderMain RangeWithTwoInputs today =
  HH.slot 3 (proxy $ ExpRangeInputs.component today) unit absurd
renderMain _ today =
  HH.article_
    [ HH.p_
        [
          HH.a
            [ HP.href "https://github.com/rnons/purescript-halogen-day-picker" ]
            [ HH.text "purescript-halogen-day-picker" ]
        , HH.text " provides DayPicker and DayPickerInput component."
        ]
    , HH.p_
        [ HH.text "Click left sidebar to view examples." ]
    ]

app :: Date -> H.Component HH.HTML Query Unit Void AppM
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

  render :: State -> H.ParentHTML Query ChildQuery Slot AppM
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
              , HH.li_
                  [ HH.a [ HP.href $ show RangeWithTwoInputs ]
                      [ HH.text "Range with two inputs" ]
                  ]
              ]
          ]
      , HH.div [ HP.class_ $ HH.ClassName "main" ]
          [ renderMain state.route today ]
      ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery Slot Void AppM
  eval (RouteChange route next) = do
    H.modify (\state -> state { route = route })
    pure next
