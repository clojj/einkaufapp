module Main exposing (..)

import Html exposing (..)
import Http exposing (stringPart, Request)
import Task
import Json.Decode as Decode exposing (Decoder, field, string, float, int, maybe)
import Json.Decode.Pipeline exposing (decode, required, optional)


type Msg = AnswerRequestCompleted (Result Http.Error Answer)

type alias Model =
    { answer : Maybe Answer
    }

type alias Answer =
    { err : String
    , word : String
    }

initialModel : Model
initialModel =
    { answer = Nothing
    }

main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, initCmd )
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.none)
        }

initCmd : Cmd Msg
initCmd =
    Http.get "/something..." answerDecoder
        |> Http.send AnswerRequestCompleted


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        AnswerRequestCompleted (Ok answer) ->
            { model | answer = Just answer } ! []

        AnswerRequestCompleted (Err err) ->
            { model | answer = Nothing } ! []


answerDecoder : Decoder Answer
answerDecoder =
    decode Answer
        |> optional "err" string "nothing"
        |> required "word" string

view : Model -> Html Msg
view model =
    div []
        [ renderAnswer model.answer ]

renderAnswer : Maybe Answer -> Html msg
renderAnswer answer =
    case answer of
        Nothing -> div [] []
        Just a -> text (toString a.word)

