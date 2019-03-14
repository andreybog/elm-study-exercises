module Main exposing (either, find, keepOks, mapOk, maybeToList, updateList, updateListKv)

{-
   > maybeToList (Just 3)
   [3]
   > maybeToList Nothing
   []
-}


maybeToList : Maybe a -> List a
maybeToList x =
    case x of
        Just value ->
            [ value ]

        Nothing ->
            []



{-
   updateList

   Change or remove element if it matches the shouldChange test.

   updateList : (a -> Bool) -> (a -> Maybe a) -> List a -> List a
   updateList shouldChange f xs = Debug.todo ""

   > updateList (\x -> x == 3) (\v -> Just (v + 1)) [1,3,5]
   [1,4,5] : List number
   > updateList (\x -> x == 3) (\v -> Nothing) [1,3,5]
   [1,5] : List number
-}


updateList : (a -> Bool) -> (a -> Maybe a) -> List a -> List a
updateList shouldChange f xs =
    List.filterMap
        (\x ->
            if shouldChange x then
                f x

            else
                Just x
        )
        xs



{-
   find

   find : (a -> Bool) -> List a -> Maybe a
   find f xs = Debug.todo ""

   > find (\x -> x == 2) [1,3,5,2]
   Just 2 : Maybe number
   > find (\x -> x == 2) [1,3,5]
   Nothing : Maybe number
-}


find : (a -> Bool) -> List a -> Maybe a
find f xs =
    case xs of
        x :: tail ->
            if f x then
                Just x

            else
                find f tail

        [] ->
            Nothing



{-
   updateListKv :
     List (k, v)
     -> k
     -> (v -> Maybe v)
     -> List (k, v)
   updateListKv old k f = Debug.todo ""

   > updateListKv [("foo", 1), ("bar", 2)] "foo" (\x -> Just (x + 1))
   [("foo", 2), ("bar", 2)]
   > updateListKv [("foo", 1), ("bar", 2)] "foo" (\x -> Nothing)
   [("bar", 2)]
-}


updateListKv :
    List ( k, v )
    -> k
    -> (v -> Maybe v)
    -> List ( k, v )
updateListKv old k f =
    case old of
        ( oldK, oldV ) :: tail ->
            let
                updateListKvTail =
                    updateListKv tail k f
            in
            case ( oldK == k, f oldV ) of
                ( True, Just newV ) ->
                    ( oldK, newV ) :: updateListKvTail

                ( True, Nothing ) ->
                    updateListKvTail

                ( False, _ ) ->
                    ( oldK, oldV ) :: updateListKvTail

        [] ->
            []



{-
   keepOks : List (Result a b) -> List b
   keepOks xss = Debug.todo ""

   > keepOks [Ok 1, Err "bad", Ok 2]
   [1,2] : List number
-}


keepOks : List (Result a b) -> List b
keepOks xss =
    case xss of
        [] ->
            []

        x :: xs ->
            case x of
                Err _ ->
                    keepOks xs

                Ok v ->
                    v :: keepOks xs



{-
   mapOk : (b -> c) -> Result a b -> Result a c
   mapOk f res = Debug.todo ""

   > mapOk (\x -> x + 1) (Ok 2)
   Ok 3 : Result a number
   > mapOk (\x -> x + 1) (Err "str")
   Err "str" : Result String number
-}


mapOk : (b -> c) -> Result a b -> Result a c
mapOk f res =
    case res of
        Ok v ->
            Ok (f v)

        Err err ->
            Err err



{-
   either : (a -> c) -> (b -> c) -> Result a b -> c
   either fa fb res = Debug.todo

   > either (\x -> x + 1) (\x -> x - 1) (Ok 1)
   0 : number
   > either (\x -> x + 1) (\x -> x - 1) (Err 1)
   2 : number
-}


either : (a -> c) -> (b -> c) -> Result a b -> c
either fa fb res =
    case res of
        Ok v ->
            fb v

        Err e ->
            fa e
