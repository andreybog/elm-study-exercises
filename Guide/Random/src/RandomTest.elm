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


type alias FacePair =
    { one : Int
    , two : Int
    }


type Msg
    = Roll
    | GotNewFaces FacePair


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model
            , Random.generate GotNewFaces facePairGenerator
            )

        GotNewFaces newFace ->
            ( Model newFace.one newFace.two
            , Cmd.none
            )


roll : Random.Generator Int
roll =
    Random.int 1 6


usuallySix : Random.Generator Int
usuallySix =
    Random.weighted
        ( 10, 1 )
        [ ( 10, 2 )
        , ( 10, 3 )
        , ( 10, 4 )
        , ( 10, 5 )
        , ( 80, 6 )
        ]


facePairGenerator : Random.Generator FacePair
facePairGenerator =
    Random.map2 FacePair roll usuallySix



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        dieFace1 = String.fromInt model.dieFace1
        dieFace2 = String.fromInt model.dieFace2
    in
    div []
        [ h1 [] [ text (dieFace1 ++ " " ++ dieFace2) ]
        , button [ onClick Roll ] [ text "Roll" ]
        ]
