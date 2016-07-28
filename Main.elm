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
    [ "http://yui.yahooapis.com/pure/0.5.0/pure-min.css"
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
        , Html.form [ class "pure-form" ] [ input [ placeholder "Texto de serci ", onInput NewContent, myStyle, class "pure-input-rounded" ] [] ]
          -- , button [ onClick Search ] [ text "serci" ]
          -- , div [ myStyle ] [ text (String.reverse content.query) ]
        , div [ class "pure-g" ]
            [ div [ class "pure-u-1-12" ] []
            , div [ class "pure-u-11-12" ] (List.map vocaView content.result)
            ]
          -- , trialView (decodeString dicDecoder """ [{"lab":false,"root":[],"fix":[],"spell":"beta","descript":"2.1  2016-7-24"},{"lab":false,"root":[],"fix":[],"spell":"A","descript":"prep. 到...；去...；给...；对...；对...（的利益有影响）"}] """)
        ]


trialView rs =
    case rs of
        Ok v ->
            div [] (List.map vocaView v)

        Err e ->
            div [] [ text e ]


vocaView : Vocabulary -> Html Msg
vocaView voca =
    div [ class "pure-g" ]
        [ div [ class "pure-u-9-24" ] [ div [ myStyle ] [ text voca.spell ] ]
        , div [ class "pure-u-15-24" ]
            [ h4 [] [ text voca.des ]
            , div [] (List.map (\x -> button [ class "pure-button" ] [ text x ]) voca.root)
            ]
        , div [] []
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
    let
        dic =
            "https://simplingo.github.io/Beta2.1.json"
    in
        Task.perform FetchFail FetchSuccess (Http.get (Json.Decode.map (queryW word) dicDecoder) dic)


queryW : String -> List Vocabulary -> List Vocabulary
queryW word vocas =
    List.filter (\voca -> String.contains word voca.spell || String.contains word voca.des) vocas
