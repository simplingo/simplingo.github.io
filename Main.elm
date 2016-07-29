module Main exposing (..)

import Html exposing (..)
import Html.App exposing (beginnerProgram, program)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Json.Decode exposing (..)
import Http
import Task
import List
import String
import Css


imports =
    [ "https://cdnjs.cloudflare.com/ajax/libs/pure/0.6.0/pure-min.css"
    , "https://cdnjs.cloudflare.com/ajax/libs/materialize/0.97.7/css/materialize.min.css"
      -- , "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.6.3/css/font-awesome.min.css"
      -- , "https://cdnjs.cloudflare.com/ajax/libs/material-design-iconic-font/2.2.0/css/material-design-iconic-font.min.css"
    ]


styleSheet =
    Css.stylesheet imports []


main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { query : String
    , result : List Vocabulary
    }


init : ( Model, Cmd Msg )
init =
    ( { query = "", result = [] }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NewContent String
    | FetchSuccess (List Vocabulary)
    | FetchFail Http.Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg oldContent =
    case msg of
        NewContent content ->
            ( { oldContent | query = content }, queryWords content )

        FetchFail e ->
            ( { oldContent | query = toString e }, Cmd.none )

        FetchSuccess vocas ->
            ( { oldContent | result = vocas }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view content =
    div []
        [ Css.style [ Html.Attributes.scoped True ] styleSheet
        , Html.form []
            -- [ class "pure-form" ]
            [ div [ class "input-field " ]
                [ input [ id "query", placeholder "Texto de serci ", onInput NewContent, myStyle {- , class "pure-input-rounded" -} ] []
                , label
                    [ for "query" ]
                    [ text "Serci" ]
                ]
            ]
          -- , button [ onClick Search ] [ text "serci" ]
          -- , div [ myStyle ] [ text (String.reverse content.query) ]
        , div [ class "pure-g" ]
            [ div [ class "pure-u-1-12" ] []
            , div [ class "pure-u-11-12" ] (List.map vocaViewC content.result)
            ]
          -- , trialView (decodeString dicDecoder """ [{"lab":false,"root":[],"fix":[],"spell":"beta","descript":"2.1  2016-7-24"},{"lab":false,"root":[],"fix":[],"spell":"A","descript":"prep. 到...；去...；给...；对...；对...（的利益有影响）"}] """)
        , myfooter
        ]


myfooter : Html Msg
myfooter =
    footer
        [ class "page-footer" ]
        [ div
            [ class "container" ]
            [ div
                [ class "row" ]
                [ div
                    [ class "col l6 s12" ]
                    [ h5
                        [ class "white-text" ]
                        [ text "关于" ]
                    , p [ class "grey-text text-lighten-4" ] [ text "Simplingo is a simple language." ]
                    ]
                , div
                    [ class "col l4 offset-l2 s12" ]
                    [ h5 [ class "white-text" ] [ text "Extra Links" ]
                    , ul []
                        [ li [] [ a [ class "grey-text text-lighten-3", href gramer ] [ text "Gramer" ] ]
                        , li [] [ a [ class "grey-text text-lighten-3", href simpleGramer ] [ text "Simple Gramer" ] ]
                        , li [] [ a [ class "grey-text text-lighten-3", href docDic ] [ text "Dictionary" ] ]
                        , li [] [ a [ class "grey-text text-lighten-3", href jsonDic ] [ text "Raw data" ] ]
                        ]
                    ]
                ]
            ]
        , div
            [ class "footer-copyright" ]
            [ div
                [ class "container" ]
                [ text "© 2016 Simplingo"
                , a [ class "grey-text text-lighten-4 right", href "#!" ] [ text "More Details" ]
                ]
            ]
        ]


trialView rs =
    case rs of
        Ok v ->
            div [] (List.map vocaView v)

        Err e ->
            div [] [ text e ]


vocaView : Vocabulary -> Html Msg
vocaView voca =
    div [ class "pure-g z-depth-1" ]
        [ div [ class "pure-u-9-24" ] [ div [ myStyle ] [ text voca.spell ] ]
        , div [ class "pure-u-15-24" ]
            [ h4 [] [ text voca.des ]
            , div [] (List.map (\x -> button [ class "pure-button" ] [ text x ]) voca.root)
            ]
        , div [ class "divider" ] []
        ]


vocaViewC : Vocabulary -> Html Msg
vocaViewC voca =
    div [ class "card blue-grey darken-1 z-depth-1" ]
        [ div [ class "pure-g card-content white-text" ]
            [ div [ class "pure-u-9-24" ] [ span [ class "card-title" ] [ {- i [ class "zmdi zmdi-invert-colors zmdi-hc-fw zmdi-hc-rotate-90" ] [], -} text voca.spell ] ]
            , div [ class "pure-u-15-24" ]
                [ -- i [ class "large material-icons" ] [ text "label_outline" ]
                  p [] [ text voca.des ]
                ]
            ]
        , div
            [ class "card-action" ]
            (List.map (\x -> div [ class "chip" ] [ text x ]) voca.root)
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


myStyle =
    style
        [ ( "width", "100%" )
          -- , ( "height", "80px" )
        , ( "padding", "10px 0" )
        , ( "font-size", "2em" )
        , ( "text-align", "center" )
        ]


myStyle' =
    style []


type alias Vocabulary =
    { spell : String
    , des : String
    , root : List String
    , lab : Bool
    }


dicDecoder : Decoder (List Vocabulary)
dicDecoder =
    Json.Decode.list vocaDecoder


vocaDecoder : Decoder Vocabulary
vocaDecoder =
    object4 (\s d r l -> { spell = s, des = d, root = r, lab = l })
        ("spell" := string)
        ("descript" := string)
        ("root" := Json.Decode.list string)
        ("lab" := bool)


queryWords : String -> Cmd Msg
queryWords word =
    Task.perform FetchFail FetchSuccess (Http.get (Json.Decode.map (queryW word) dicDecoder) jsonDic)


queryW : String -> List Vocabulary -> List Vocabulary
queryW word vocas =
    List.filter (\voca -> String.contains word voca.spell || String.contains word voca.des) vocas


jsonDic =
    "https://simplingo.github.io/Beta2.1.json"


simpleGramer =
    "https://simplingo.github.io/doc/simplegramerBeta2.1.pdf"


gramer =
    "https://simplingo.github.io/doc/gramerBeta2.1.pdf"


docDic =
    "https://simplingo.github.io/doc/dicBeta2.1.doc"
