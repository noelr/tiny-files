port module Main exposing(..)

import Html exposing (Html, Attribute, a, div, text, ul, li)
import Html.Attributes exposing (href, id, class, style)
import Html.Events exposing (onClick, onWithOptions)
import Http

import Json.Decode as Decode exposing (Decoder, field)

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


type alias ServerError = String


type Msg = Drop (List String)
  | Progess Percentage
  | Done HttpStatus
  | NewFiles (Result Http.Error (List ServerFile))
  | DeleteFile ServerFile
  | FileDeleted (Result Http.Error ())


type alias Model = { state : State
                   , percentage : Percentage
                   , files : List ServerFile
                   , errors : List String
                   }


init : (Model, Cmd Msg)
init = (Model Idle 0 [] [], Cmd.batch [start 0, getFiles])


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
      ({ model | errors = [(toString error)]}, Cmd.none)
    DeleteFile file ->
      (model, deleteFile file)
    FileDeleted (Ok ()) ->
      (model, getFiles)
    FileDeleted (Err error) ->
      ({ model | errors = [(toString error)]}, Cmd.none)


view : Model -> (Html Msg)
view model =
  div []
    [ errors model.errors
    , filesUl model
    , dropZone
    , progressBar model
    ]


errors : List ServerError -> (Html Msg)
errors es =
  case es of
    [] -> text ""
    xs -> ul [] (List.map (\e -> li [] [ text e ]) xs)


filesUl : Model -> (Html Msg)
filesUl model =
  ul []
  (List.map fileLi model.files)


fileLi : ServerFile -> (Html Msg)
fileLi file =
  li [] [ a [ href ("/download/" ++ file.name) ] [ text file.name ]
        , a [ style [("float", "right")], href "#", onClick (DeleteFile file) ] [ text "Delete" ]
        ]


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


getFiles : Cmd Msg
getFiles =
    (Decode.list fileDecoder)
        |> Http.get "/files"
        |> Http.send NewFiles


deleteFile : ServerFile -> Cmd Msg
deleteFile file =
  Http.send FileDeleted (delete ("/delete/" ++ file.name) Http.emptyBody)


delete : String -> Http.Body -> Http.Request ()
delete url body =
  Http.request
    { method = "DELETE"
    , headers = []
    , url = url
    , body = body
    , expect = Http.expectStringResponse (\_ -> Ok ())
    , timeout = Nothing
    , withCredentials = False
    }


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
