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

        FetchSuccess [] ->
            ( { oldContent | query = "" }, Cmd.none )

        FetchSuccess vocas ->
            ( { oldContent | result = vocas }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view content =
    div []
        [ input [ placeholder "Text de serci ", onInput NewContent, myStyle ] []
          -- , button [ onClick Search ] [ text "serci" ]
          -- , div [ myStyle ] [ text (String.reverse content.query) ]
        , div [] (List.map vocaView content.result)
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
    div []
        [ h2 [] [ text voca.spell ]
        , h4 [] [ text voca.des ]
        , h5 [] (List.map text voca.root)
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


myStyle =
    style
        [ ( "width", "100%" )
        , ( "height", "40px" )
        , ( "padding", "10px 0" )
        , ( "font-size", "2em" )
        , ( "text-align", "center" )
        ]


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
