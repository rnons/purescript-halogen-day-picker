module App where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Date (Date)

import Data.Const (Const)
import Data.Functor (mapFlipped)
import Data.StrMap as SM
import Data.Tuple (Tuple(Tuple))

import Halogen as H
import Halogen.Component.Proxy (ProxyQ, proxy)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Global (decodeURI, encodeURI)

import Examples.Types (AppM)
import Examples.Simple as ExpSimple
import Examples.SimpleInput as ExpSimpleInput
import Examples.RangeWithTwoInputs as ExpRangeInputs

data Query a
  = RouteChange String a

type State =
  { selectedDate :: Maybe Date
  , route :: String
  }

type ChildQuery = ProxyQ (Const Void) Unit Void

type Slot = String

type StoryMap q i o m = SM.StrMap (H.Component HH.HTML q i o m)

stories :: Date -> StoryMap ChildQuery Unit Void AppM
stories today = SM.fromFoldable
  [ Tuple "Simple day picker" $ proxy $ ExpSimple.component today
  , Tuple "Simple day picker input" $ proxy $ ExpSimpleInput.component today
  , Tuple "Range with two inputs" $ proxy $ ExpRangeInputs.component today
  ]

renderSidebar :: Date -> H.ParentHTML Query ChildQuery Slot AppM
renderSidebar today =
  HH.ul_ $
    mapFlipped (SM.keys $ stories today) $ \name ->
      HH.li_
      [ HH.a
        [ HP.href $ "#" <> encodeURI name ]
        [ HH.text name ]
      ]

renderMain :: String -> Date -> H.ParentHTML Query ChildQuery Slot AppM
renderMain route today =
  case SM.lookup decoded (stories today) of
    Just cmp -> HH.slot decoded cmp unit absurd
    _ ->
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
  where
    decoded = decodeURI route

render :: Date -> State -> H.ParentHTML Query ChildQuery Slot AppM
render today state =
  HH.div [ HP.class_ $ HH.ClassName "container" ]
    [ HH.div [ HP.class_ $ HH.ClassName "sidebar" ]
        [ HH.a [ HP.href "" ]
            [ HH.text "Home" ]
        , renderSidebar today
        ]
    , HH.div [ HP.class_ $ HH.ClassName "main" ]
        [ renderMain state.route today ]
    ]

app :: Date -> H.Component HH.HTML Query Unit Void AppM
app today =
  H.parentComponent
    { initialState: const initialState
    , render: render today
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { selectedDate: Nothing, route: "" }

  eval :: Query ~> H.ParentDSL State Query ChildQuery Slot Void AppM
  eval (RouteChange route next) = do
    H.modify (\state -> state { route = route })
    pure next
