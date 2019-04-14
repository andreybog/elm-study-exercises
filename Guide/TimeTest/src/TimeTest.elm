module TimeTest exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , isTimerOn : Bool
    }


init : Int -> ( Model, Cmd Msg )
init flag =
    ( Model Time.utc (Time.millisToPosix flag) True
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | StopResumeTimer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        StopResumeTimer ->
            ( { model | isTimerOn = not model.isTimerOn }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isTimerOn then
        Time.every 1000 Tick

    else
        Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        getComponent : (Time.Zone -> Time.Posix -> Int) -> String
        getComponent f =
            String.fromInt (f model.zone model.time)
                |> String.padLeft 2 '0'

        hour =
            getComponent Time.toHour

        minute =
            getComponent Time.toMinute

        second =
            getComponent Time.toSecond
    in
    h1 []
        [ text (hour ++ ":" ++ minute ++ ":" ++ second)
        , div [] [ timerButton model ]
        ]


timerButton : Model -> Html Msg
timerButton model =
    button [ onClick StopResumeTimer ]
        [ text
            (if model.isTimerOn then
                "Stop"

             else
                "Resume"
            )
        ]
