module GUI
(mainGui)
where

import Prelude (Unit, bind, void, ($), (<$>), (<<<), (<>), (>>=))


import Control.Monad.Eff (Eff)

import Data.String (charAt)
import ReactDOM (render) as R
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (Maybe(..))
import Data.Array (singleton, take)
import Thermite (PerformAction, Render, Spec, createClass, simpleSpec, writeState)
import Data.Nullable (toMaybe)
import Data.Foldable (traverse_)

import DOM (DOM) as DOM
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToNonElementParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.NonElementParentNode (getElementById) as DOM
import DOM.Node.Types (ElementId(ElementId)) as DOM
import React (createFactory, ReactElement) as R
import React.DOM (a, blockquote, div, footer, form', h5, header', input, label, li, main', nav', p, span, text, ul) as R
import React.DOM.Props as RP

import Dictionary (Dictionary(..), queryDict)

type MyState = Array Dictionary

data MyAction = Query String

render :: forall props. Render MyState props MyAction
render dispatch _ dicts _ =
  let
    container = R.div [RP.className "container"]
    inputArea = R.form' <<< singleton $ R.div [RP.className "input-field"] $
      [ R.input [RP.className "center-align validate", RP._id "search", RP.typeof "search", RP.required true, RP.placeholder "Lexo de serka", RP.onInput \e -> dispatch (Query $ (unsafeCoerce e).target.value)] []
      , R.label [RP.property "for=\"search\""] [R.text "SIMPLINGO"]
      ]
    resultList = R.div [RP.className "col s4"] <<< singleton <<< renderDictionary <$> dicts
    addHeader = R.header' <<< singleton
    navi = R.nav' <<< singleton <<< R.div [RP.className "nav-wrapper"]  <<< singleton -- R.div [RP.className "row"] [R.div [RP.className "col s3"] [l], R.div [RP.className "col s9"] [i]]
    mainBody = R.main' <<< singleton $ container $ [R.div [RP.className "row"] resultList]
    -- logo = R.a [RP.className "brand-logo"] [R.text "Simplingo"]
    links = linkList  "grammar.pdf" "pronunciation.pdf" "simplingo.xlsx"
  in
    [navi inputArea, mainBody, myFooter links]

renderDictionary :: Dictionary -> R.ReactElement
renderDictionary (Dictionary dic) =
  let
    r = case dic.root of
      Nothing -> []
      Just rr -> [R.div [ RP.className "card-action" ]
                     [R.div [ RP.className "chip" ] [R.text rr]]]
    reColor = case charAt 0 dic.spell of
      Just '/' -> "grey"
      _ -> "blue-grey"
  in
    -- [R.p [] [R.text dic.spell] , R.p' $ [R.q' [R.text dic.meanning]] <> r]
    R.blockquote [ RP.className "red lighten-2 center-aligh" ]
      [ R.div [ RP.className $ "card " <> reColor <> " darken-1 hoverable" ] $
          [ R.div [ RP.className "pure-g card-content white-text" ]
            [ R.span [ RP.className "card-title" ] [ R.text dic.spell ]
            , R.p [ RP.className "text-darken-2" ] [ R.text dic.meanning ]
            ]
          ] <> r
      ]


myFooter :: R.ReactElement -> R.ReactElement
myFooter links =
    R.footer [ RP.className "page-footer" ]
        [ R.div [ RP.className "container" ]
            [ R.div [ RP.className "row" ]
                [ R.div [ RP.className "col l6 s12" ]
                    [ R.h5 [ RP.className "white-text" ] [ R.text "About" ]
                    , R.p [ RP.className "grey-text text-lighten-4" ] [ R.text "Simplingo is a simple language." ]
                    ]
                , R.div
                    [ RP.className "col l4 offset-l2 s12" ]
                    [ R.h5 [ RP.className "white-text" ] [ R.text "Extra Links" ]
                    , links
                    ]
                ]
            ]
        , R.div [ RP.className "footer-copyright" ]
            [ R.div [ RP.className "container" ]
                [ R.text "© 2016-2017 Simplingo 1.0"
                , R.a [ RP.className "grey-text text-lighten-4 right", RP.href "#!" ] [ R.text "More Details" ]
                ]
            ]
        ]

linkList :: String -> String -> String -> R.ReactElement
linkList grammar pronunciation docDic = R.ul []
  [ R.li [] [ R.a [ RP.className "grey-text text-lighten-3", RP.href grammar ] [ R.text "Grammar" ] ]
  , R.li [] [ R.a [ RP.className "grey-text text-lighten-3", RP.href pronunciation ] [ R.text "Pronunciation" ] ]
  , R.li [] [ R.a [ RP.className "grey-text text-lighten-3", RP.href docDic ] [ R.text "Dictionary" ] ]
  ]

mainGui
  :: forall eff
  .  Array Dictionary
  -> Eff (dom :: DOM.DOM | eff) Unit
mainGui dict =
  myMain spec initialState ""
    where

      performAction :: forall e p . PerformAction e MyState p MyAction
      performAction (Query queryString) _ _ = void $ writeState $ take 100 $ queryDict queryString dict  -- queryString

      initialState :: MyState
      initialState = []

      spec :: forall e p . Spec e MyState p MyAction
      spec = simpleSpec performAction render

      myMain s ss props = void do
          let component = createClass s ss
          doc <- DOM.window >>= DOM.document
          con <- toMaybe <$> DOM.getElementById (DOM.ElementId "app") (DOM.htmlDocumentToNonElementParentNode doc)
          traverse_ (R.render (R.createFactory component props)) con
