port module Main exposing(..)

import Html exposing (Html, Attribute, a, div, text)
import Html.Attributes exposing (href, id, class, style)
import Html.Events exposing (onClick, onWithOptions)

import Json.Decode as Json

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias HttpStatus = Int


type alias Percentage = Int


type State = Idle | Loading


type Msg = Drop (List String) | Progess Percentage | Done HttpStatus


type alias Model = { state : State, percentage : Percentage }


init : (Model, Cmd Msg)
init = (Model Idle 0, start 0)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Drop _ ->
      ({ model | percentage = 0, state = Loading }, Cmd.none)
    Progess percentage ->
      ({ model | percentage = percentage }, Cmd.none)
    Done _ ->
      ({ model | percentage = 100, state = Idle }, Cmd.none)


view : Model -> (Html Msg)
view model =
  div []
    [ a [ href "/download" ] [ text "Download" ]
    , div [ id "drop_zone" ] [ text "Drop files here" ]
    , div [ id "progress_bar", class (classForProgressbar model.state) ]
          [ div [ class "percent", style [ ("width", (toPercentString model.percentage)) ] ]
                [ text (toPercentString model.percentage) ]
          ]
    ]


toPercentString : Percentage -> String
toPercentString percentage =
  (toString percentage) ++ "%"


classForProgressbar : State -> String
classForProgressbar state =
  case state of
    Idle -> ""
    Loading -> "loading"


port start : Int -> Cmd msg


port drop : (List String -> msg) -> Sub msg


port progress : (Percentage -> msg) -> Sub msg


port done : (HttpStatus -> msg) -> Sub msg


subscriptions model =
  Sub.batch
    [ drop Drop
    , progress Progess
    , done Done
    ]
