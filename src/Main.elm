module Main exposing (main)

import Array exposing (..)
import Browser
import Html exposing (Attribute, Html, button, div, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (reverse)
import Random



-- MAIN


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subs }



-- MODEL


type alias Model =
    { gameState : Array Int
    , score : Int
    , message : String
    , run : Bool
    }


init : flags -> ( Model, Cmd Msg )
init flags =
    ( { gameState = fromList [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
      , score = 0
      , message = ""
      , run = True
      }
    , Random.generate GeneratedPos (Random.int 1 16) 
    )



-- UPDATE


type Msg
    = Left
    | Right
    | Up
    | Down
    | Generated Int Int
    | GeneratedPos Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        gsLength =
            (length model.gameState) - 1
    in
    if model.run == True then
        case msg of
          Left ->
              ( updateHelper msg model, Random.generate GeneratedPos (Random.int 0 gsLength) )
  
          Right ->
              ( updateHelper msg model, Random.generate GeneratedPos (Random.int 0 gsLength) )
  
          Up ->
              ( updateHelper msg model, Random.generate GeneratedPos (Random.int 0 gsLength) )
  
          Down ->
              ( updateHelper msg model, Random.generate GeneratedPos (Random.int 0 gsLength) )
  
          Generated new pos ->
              if length (filter (\x -> x == 0) model.gameState) == (gsLength + 1) then
                ( updateHelper msg model, Random.generate GeneratedPos (Random.int 0 gsLength) )
              else if length (filter (\x -> x >= 2048) model.gameState) > 0 then
                ( stopRun (updateHelper msg model) " You win!", Cmd.none )
              else
                ( updateHelper msg model, Cmd.none )
              
          GeneratedPos pos ->
            if getInt pos model.gameState == 0 then
              ( updateHelper msg model, Random.generate (\x -> Generated x pos) (Random.int 1 10) )
            else
              if length (filter (\x -> x == 0) model.gameState) > 0 then
                ( model, Random.generate GeneratedPos (Random.int 0 gsLength) )
              else
                ( stopRun (updateHelper msg model) " You lose!", Cmd.none )
              
      else
        (model, Cmd.none)


updateModel : (Array Int, Int) -> Model -> Model
updateModel ( newGameState, newScore ) model =
    { model | gameState = newGameState, score = model.score + newScore }


updateHelper : Msg -> Model -> Model
updateHelper msg model =
    case msg of
        Left ->
            updateModel (moveLeft model.gameState) model

        Right ->
            updateModel (moveRight model.gameState) model

        Up ->
            updateModel (moveUp model.gameState) model

        Down ->
            updateModel (moveDown model.gameState) model

        Generated new pos ->
            { model | gameState = add new pos model.gameState }

        GeneratedPos new ->
            model

stopRun : Model -> String -> Model
stopRun model message = 
  { model | message = message, run = False }

add : Int -> Int -> Array Int -> Array Int
add new pos gs =
    if new < 10 then
        set pos 2 gs
    else
        set pos 4 gs


moveLeft : Array Int -> ( Array Int, Int )
moveLeft gs =
    let
        ( row1, sc1 ) =
            pushRow (slice 0 4 gs) 0

        ( row2, sc2 ) =
            pushRow (slice 4 8 gs) 0

        ( row3, sc3 ) =
            pushRow (slice 8 12 gs) 0

        ( row4, sc4 ) =
            pushRow (slice 12 16 gs) 0
    in
    ( append row1 (append row2 (append row3 row4)), sc1 + sc2 + sc3 + sc4 )


moveRight : Array Int -> ( Array Int, Int )
moveRight gs =
    let
        ( row1, sc1 ) =
            pushRow (reverseOfSlice gs 0 4) 0

        ( row2, sc2 ) =
            pushRow (reverseOfSlice gs 4 8) 0

        ( row3, sc3 ) =
            pushRow (reverseOfSlice gs 8 12) 0

        ( row4, sc4 ) =
            pushRow (reverseOfSlice gs 12 16) 0
    in
    ( append (reverseArray row1) (append (reverseArray row2) (append (reverseArray row3) (reverseArray row4))), sc1 + sc2 + sc3 + sc4 )


moveUp : Array Int -> ( Array Int, Int )
moveUp gs =
    let
        rows =
            seperateColumns gs

        col1 =
            getArr 0 rows

        col2 =
            getArr 1 rows

        col3 =
            getArr 2 rows

        col4 =
            getArr 3 rows

        ( result, sc ) =
            moveLeft (append col1 (append col2 (append col3 col4)))
    in
    ( fromList (List.map (\x -> getInt x result) [ 0, 4, 8, 12, 1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15 ]), sc )


moveDown : Array Int -> ( Array Int, Int )
moveDown gs =
    let
        rows =
            seperateColumns gs

        col1 =
            getArr 0 rows

        col2 =
            getArr 1 rows

        col3 =
            getArr 2 rows

        col4 =
            getArr 3 rows

        ( result, sc ) =
            moveRight (append col1 (append col2 (append col3 col4)))
    in
    ( fromList (List.map (\x -> getInt x result) [ 0, 4, 8, 12, 1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15 ]), sc )


pushRow : Array Int -> Int -> ( Array Int, Int )
pushRow row sc =
    let
        arr =
            fromList (List.map (\x -> getInt x row) [ 0, 1, 2, 3 ])

        filtered =
            filter isNotZero arr

        ( smushed, score ) =
            checkSame (fromList []) filtered sc

        withZeros =
            addZeros smushed
    in
    ( withZeros, sc + score )


isNotZero : Int -> Bool
isNotZero i =
    not (i == 0)


checkSame : Array Int -> Array Int -> Int -> ( Array Int, Int )
checkSame outputArray inputArray sc =
    let
        inputList =
            toList inputArray

        outputList =
            toList outputArray
    in
    case inputList of
        x1 :: x2 :: xs ->
            if x1 == x2 then
                checkSame (push (x1 * 2) outputArray) (slice 2 (length inputArray) inputArray) (sc + (x1 * 2))

            else
                checkSame (push x1 outputArray) (slice 1 (length inputArray) inputArray) sc

        _ ->
            ( append outputArray inputArray, sc )


addZeros : Array Int -> Array Int
addZeros array =
    if length array == 4 then
        array

    else
        addZeros (push 0 array)



-- SUBS


subs : Model -> Sub Msg
subs model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "game" ]
        [ div [ class "score "] 
          [ div [] [ text ((String.fromInt model.score) ++ model.message) ]
          ]
        , div [ class "rows" ]
            [ div [ class "row row1" ]
                [ getGameSquare 0 model.gameState
                , getGameSquare 1 model.gameState
                , getGameSquare 2 model.gameState
                , getGameSquare 3 model.gameState
                ]
            , div [ class "row row2" ]
                [ getGameSquare 4 model.gameState
                , getGameSquare 5 model.gameState
                , getGameSquare 6 model.gameState
                , getGameSquare 7 model.gameState
                ]
            , div [ class "row row3" ]
                [ getGameSquare 8 model.gameState
                , getGameSquare 9 model.gameState
                , getGameSquare 10 model.gameState
                , getGameSquare 11 model.gameState
                ]
            , div [ class "row row3" ]
                [ getGameSquare 12 model.gameState
                , getGameSquare 13 model.gameState
                , getGameSquare 14 model.gameState
                , getGameSquare 15 model.gameState
                ]
            , div [ class "buttons" ]
                [ button [ onClick Left ] [ text "Left" ]
                , button [ onClick Right ] [ text "Right" ]
                ]
            , div [ class "buttons" ]
                [ button [ onClick Up ] [ text "Up" ]
                , button [ onClick Down ] [ text "Down" ]
                ]
            ]
        
        ]



-- UTILITY


getInt : Int -> Array Int -> Int
getInt pos arr =
    case get pos arr of
        Just a ->
            a

        Nothing ->
            -1


getArr : Int -> Array (Array a) -> Array a
getArr pos arr =
    case get pos arr of
        Just a ->
            a

        Nothing ->
            fromList []


getGameSquare : Int -> Array Int -> Html Msg
getGameSquare pos gameState =
    span [ classList [ ( "number", not (getInt pos gameState == 0) ) ], style "--bgcolor" "#f00" ] [ text (String.fromInt (getInt pos gameState)) ]


seperateColumns : Array Int -> Array (Array Int)
seperateColumns gs =
    let
        row1 =
            fromList [ getInt 0 gs, getInt 4 gs, getInt 8 gs, getInt 12 gs ]

        row2 =
            fromList [ getInt 1 gs, getInt 5 gs, getInt 9 gs, getInt 13 gs ]

        row3 =
            fromList [ getInt 2 gs, getInt 6 gs, getInt 10 gs, getInt 14 gs ]

        row4 =
            fromList [ getInt 3 gs, getInt 7 gs, getInt 11 gs, getInt 15 gs ]
    in
    fromList [ row1, row2, row3, row4 ]


reverseOfSlice : Array a -> Int -> Int -> Array a
reverseOfSlice gs pos1 pos2 =
    fromList (reverse (toList (slice pos1 pos2 gs)))


reverseArray : Array a -> Array a
reverseArray arr =
    fromList (reverse (toList arr))
