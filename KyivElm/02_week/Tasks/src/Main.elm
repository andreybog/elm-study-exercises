module Main exposing (Address, Profile, User, bird, bird2, bird3, buildStatsUrl, catMaybes, convert, convert02, convert03, filterMap, filterMap2, map, setPhone)

import Url.Builder exposing (QueryParameter, int, string)


catMaybes list =
    case list of
        [] ->
            []

        (Just x) :: tail ->
            x :: catMaybes tail

        Nothing :: tail ->
            catMaybes tail


filterMap2 f list =
    case list of
        x :: tail ->
            case f x of
                Just result ->
                    result :: filterMap f tail

                Nothing ->
                    filterMap f tail

        [] ->
            []


map f list =
    case list of
        [] ->
            []

        x :: tail ->
            f x :: map f tail


filterMap : (a -> Maybe b) -> List a -> List b
filterMap f list =
    list |> map f |> catMaybes



{-
   Map one structure to another

   convert
     : List { name : String, email : String, phone_number : String}
     -> List { name : String, email : String}
   > convert [{name="John", email="john@gmail.com", phone_number="+3801234567"}]
   [{name="John", email="john@gmail.com"}]
-}


convert list =
    map (\x -> { name = x.name, email = x.email }) list



{-
   Filter elements with non-empty name and email

   convert02
     : List { name : Maybe String, email : Maybe String}
     -> List { name : String, email : String}
   > convert02 [{name=Just "John", email=Just "john@gmail.com"}]
   [{name="John", email="john@gmail.com"}]
-}


convert02 list =
    let
        filter x =
            case ( x.name, x.email ) of
                ( Just n, Just e ) ->
                    Just { name = n, email = e }

                _ ->
                    Nothing
    in
    filterMap filter list



{-
   Fill in missing names with <unspecified>, while removing elements with no email

   convert03
     : List { name : Maybe String, email : Maybe String}
     -> List { name : String, email : String}
   > convert03 [{name=Just "John", email=Nothing}]
   [{name="John", email="<unspecified>"}]
-}


convert03 list =
    let
        filter x =
            case ( x.name, x.email ) of
                ( Just n, Just e ) ->
                    Just { name = n, email = e }

                ( Just n, _ ) ->
                    Just { name = n, email = "<unspecified>" }

                _ ->
                    Nothing
    in
    filterMap filter list



{-
   Rewrite bird using <|, then using |> instead of parens (where applicable)

   bird : Int
   bird =
       let
           notThree x =
               x /= 3

           incr x =
               x + 1
       in
       List.sum (List.filter notThree (List.map incr [ 1, 2, 3 ]))

   -- using <|
   bird2 = Debug.todo ""

   -- using |>
   bird3 = Debug.todo ""
-}


bird =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.sum (List.filter notThree (List.map incr [ 1, 2, 3 ]))


bird2 =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    List.sum <|
        List.filter notThree <|
            List.map incr <|
                [ 1, 2, 3 ]


bird3 =
    let
        notThree x =
            x /= 3

        incr x =
            x + 1
    in
    [ 1, 2, 3 ]
        |> List.map incr
        |> List.filter notThree
        |> List.sum



{-
   Implement setPhone

   type alias User = { profile : Profile }
   type alias Profile = { address : Address }
   type alias Address = { phone : String }

   setPhone : String -> User -> User
   setPhone = Debug.todo ""

   > setPhone "+123456" { profile = { address = { phone = "+654321" } } }
   { profile = { address = { phone = "+123456" } } }
-}


type alias User =
    { profile : Profile }


type alias Profile =
    { address : Address }


type alias Address =
    { phone : String }


setPhone : String -> User -> User
setPhone phone user =
    { user | profile = { address = { phone = phone } } }



{-
   Use package elm/url and its Url.Builder.absolute to build URL from parameters

   buildStatsUrl : Int -> { startDate : Maybe String, numElems : Maybe Int } -> String
   buildStatsUrl itemId ps =
     Debug.todo ""

   > buildStatsUrl 12 {startDate=Nothing, numElems=Nothing}
   https://myapi.com/api/item/12/stats.json
   > buildStatsUrl 12 {startDate=Just "2019-01-01", numElems=Nothing}
   https://myapi.com/api/item/12/stats.json?start_date=2019-01-01
   > buildStatsUrl 12 {startDate=Just "2019-01-01", numElems=Just 10}
   https://myapi.com/api/item/12/stats.json?start_date=2019-01-01&num_items=10
-}


buildStatsUrl : Int -> { startDate : Maybe String, numElems : Maybe Int } -> String
buildStatsUrl itemId ps =
    let
        stringParams =
            [ { key = "startDate", value = ps.startDate } ]

        intParams =
            [ { key = "numElems", value = ps.numElems } ]

        mapParams : (String -> a -> QueryParameter) -> List { key : String, value : Maybe a } -> List QueryParameter
        mapParams f list =
            filterMap
                (\x ->
                    case x.value of
                        Just v ->
                            Just (f x.key v)

                        _ ->
                            Nothing
                )
                list

        queryParams =
            mapParams string stringParams ++ mapParams int intParams
    in
    Url.Builder.crossOrigin
        "https://myapi.com"
        [ "item"
        , String.fromInt itemId
        , "stats.json"
        ]
        queryParams
