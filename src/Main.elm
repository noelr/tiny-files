port module Main exposing(..)

import Html exposing (Html, Attribute, a, div, text, ul, li)
import Html.Attributes exposing (href, id, class, style)
import Html.Events exposing (onClick, onWithOptions)
import Http

import Json.Decode as Decode exposing (Decoder, field, succeed)

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias ServerFile = { name : String }


type alias HttpStatus = Int


type alias Percentage = Int


type State = Idle | Loading


type Msg = Drop (List String) | Progess Percentage | Done HttpStatus | NewFiles (Result Http.Error (List ServerFile))


type alias Model = { state : State, percentage : Percentage, files : List ServerFile }


init : (Model, Cmd Msg)
init = (Model Idle 0 [], Cmd.batch [start 0, getFiles])


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Drop _ ->
      ({ model | percentage = 0, state = Loading }, Cmd.none)
    Progess percentage ->
      ({ model | percentage = percentage }, Cmd.none)
    Done _ ->
      ({ model | percentage = 100, state = Idle }, getFiles)
    NewFiles (Ok files) ->
      ({ model | files = files }, Cmd.none)

    NewFiles (Err error) ->
      let
        _ = Debug.log "Oops!" error
      in
        (model, Cmd.none)


view : Model -> (Html Msg)
view model =
  div []
    [ filesUl model
    , dropZone
    , progressBar model
    ]


filesUl : Model -> (Html Msg)
filesUl model =
  ul []
  (List.map (\l -> li [] [ a [href ("/download/" ++ l.name)] [ text l.name ] ]) model.files)


progressBar : Model -> Html msg
progressBar model =
  div [ id "progress_bar"
      , style [ ("margin", "10px 0")
              , ("padding", "3px")
              , ("border", "1px solid #000")
              , ("font-size", "14px")
              , ("clear", "both")
              , ("opacity", (if model.state == Idle then "0" else "1.0"))
              , ("-moz-transition", "opacity 1s linear")
              , ("-o-transition", "opacity 1s linear")
              , ("-webkit-transition", "opacity 1s linear")
              ]
      ]
      [ div [ style [ ("width", (toPercentString model.percentage))
                    , ("background-color", "#99ccff")
                    , ("height", "auto")
                    ]
            ]
            [ text (toPercentString model.percentage) ]
      ]


dropZone : Html msg
dropZone =
  div [ id "drop_zone"
      , style [("width", "100%"), ("height", "300px"), ("border", "1px dashed gray")]
      ]
      [ text "Drop files here" ]


toPercentString : Percentage -> String
toPercentString percentage =
  (toString percentage) ++ "%"


filesUrl : String
filesUrl =
    "/files"


getFiles : Cmd Msg
getFiles =
    (Decode.list fileDecoder)
        |> Http.get filesUrl
        |> Http.send NewFiles


fileDecoder : Decoder ServerFile
fileDecoder =
    Decode.map ServerFile
        (field "name" Decode.string)


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
