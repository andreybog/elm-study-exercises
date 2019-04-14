module Textfields exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , age : String
    , shouldCheck : Bool
    }


init : Model
init =
    Model "" "" "" "" False



-- UPDATE


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name, shouldCheck = False }

        Password password ->
            { model | password = password, shouldCheck = False }

        PasswordAgain password ->
            { model | passwordAgain = password, shouldCheck = False }

        Age age ->
            { model | age = age, shouldCheck = False }

        Submit ->
            { model | shouldCheck = True }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewInput "text" "Name" model.name Name
        , viewInput "text" "Age" model.age Age
        , viewInput "password" "Password" model.password Password
        , viewInput "password" "Re-enter Password" model.passwordAgain PasswordAgain
        , div [] [ button [ onClick Submit ] [ text "Submit" ] ]
        , viewValidation model
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []


viewValidation : Model -> Html msg
viewValidation model =
    if model.shouldCheck == False then
        div [] []

    else if model.password /= model.passwordAgain then
        makeValidationView "red" "Passwords do not match!"

    else
        case isAgeValid model.age of
            Err error ->
                makeValidationView "red" error

            Ok _ ->
                case validatePassword model.password of
                    ( _, Just error ) ->
                        makeValidationView "red" error

                    ( _, Nothing ) ->
                        makeValidationView "green" "OK"


makeValidationView : String -> String -> Html msg
makeValidationView color message =
    div [ style "color" color ] [ text message ]


isAgeValid : String -> Result String Int
isAgeValid age =
    case String.toInt age of
        Just n ->
            if n < 0 then
                Err "Can't be negative age"

            else if n > 135 then
                Err "Age is to big"

            else
                Ok n

        Nothing ->
            Err "Age is not a number"


validatePassword : String -> ( Bool, Maybe String )
validatePassword password =
    if not (isPasswordLongEnough password) then
        ( False, Just "Password should be at least 8 characters" )

    else if not (isPasswordContainsNeededChars password) then
        ( False, Just "Password should contain upper case, lower case, and numeric characters" )

    else
        ( True, Nothing )


isPasswordLongEnough : String -> Bool
isPasswordLongEnough password =
    if String.length password < 8 then
        False

    else
        True


isPasswordContainsNeededChars : String -> Bool
isPasswordContainsNeededChars password =
    let
        containsDigit =
            String.any Char.isDigit

        containsLower =
            String.any Char.isLower

        containsUpper =
            String.any Char.isUpper
    in
    containsLower password && containsUpper password && containsDigit password
