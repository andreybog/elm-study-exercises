module TempCalculator exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


view model =
    div [ class "content" ]
        [ h1 [] [ text "Temp Calculator" ]
        , div []
            [ text "Celsius"
            , input [ placeholder "Enter", value model.celsius, onInput CelsiusChange ] []
            ]
        , div []
            [ text "Fahrenheit"
            , input [ placeholder "Enter", value model.fahrenheit, onInput FahrenheitChange ] []
            ]
        ]


type alias Model =
    { celsius : String
    , fahrenheit : String
    }


initialModel : Model
initialModel =
    { celsius = ""
    , fahrenheit = ""
    }


type Msg
    = CelsiusChange String
    | FahrenheitChange String


update : Msg -> Model -> Model
update msg model =
    case msg of
        CelsiusChange value ->
            case String.toFloat value of
                Just celsius ->
                    { model
                        | celsius = value
                        , fahrenheit = String.fromFloat << fahrenheitFromCelsius <| celsius
                    }

                Nothing ->
                    { model | celsius = value }

        FahrenheitChange value ->
            case String.toFloat value of
                Just fahrenheit ->
                    { model
                        | celsius = String.fromFloat << celsiusFromFahrenheit <| fahrenheit
                        , fahrenheit = value
                    }

                Nothing ->
                    { model | fahrenheit = value }


celsiusFromFahrenheit : Float -> Float
celsiusFromFahrenheit f =
    (f - 32) / 1.8


fahrenheitFromCelsius : Float -> Float
fahrenheitFromCelsius c =
    c * 1.8 + 32


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
