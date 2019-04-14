module RandomTest exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (..)
import Random



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { dieFace1 : Int
    , dieFace2 : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 1 1
    , Cmd.none
    )



-- UPDATE


type alias Face =
    { one : Int
    , two : Int
    }


type Msg
    = Roll
    | NewFaces Face


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate NewFaces (Random.map2 Face (Random.int 1 6) (Random.int 1 6))
            )

        NewFaces newFace ->
            ( Model newFace.one newFace.two
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ text
                (String.fromInt model.dieFace1
                    ++ " "
                    ++ String.fromInt model.dieFace2
                )
            ]
        , button [ onClick Roll ] [ text "Roll" ]
        ]
