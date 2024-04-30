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
    ,  score : Int
    }


init : flags -> (Model, Cmd Msg)
init flags =
    ({ gameState = fromList [ 0, 0, 2, 0, 2, 4, 4, 0, 4 ] 
    , score = 0
    },  Cmd.none)



-- UPDATE


type Msg
    = Left
    | Right
    | Up
    | Down
    | Generated Int Int
    | GeneratedPos Int


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Left -> (updateHelper msg model, Random.generate (\x -> GeneratedPos x) (Random.int 1 9))
    Right -> (updateHelper msg model, Random.generate (\x -> GeneratedPos x) (Random.int 1 9))
    Up -> (updateHelper msg model, Random.generate (\x -> GeneratedPos x) (Random.int 1 9))
    Down -> (updateHelper msg model, Random.generate (\x -> GeneratedPos x) (Random.int 1 9))
    GeneratedPos pos -> (updateHelper msg model, Random.generate (\x -> Generated x pos) (Random.int 1 9))
    other -> (updateHelper msg model, Cmd.none)


updateModel (newGameState, newScore) model =
  { model | gameState = newGameState, score = model.score + newScore }
  

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

getZeroPos : Int -> Array Int -> Int -> Int
getZeroPos pos gs count = 
  if count < 9 then
    if Maybe.withDefault -1 (get pos gs) == 0 then 
      pos 
    else 
      getZeroPos (if pos - 1 == -1 then 9 else pos - 1) gs (count + 1)
  else
    -1

add : Int -> Int -> Array Int -> Array Int
add new pos gs =
  let
    changePos = getZeroPos pos gs 0
  in
    if new < 9 then 
      set changePos 2 gs
    else
      set changePos 4 gs

moveLeft : Array Int -> (Array Int, Int)
moveLeft gs =
    let
        (row1, sc1) =
            pushRow (slice 0 3 gs) 0

        (row2, sc2) =
            pushRow (slice 3 6 gs) 0

        (row3, sc3) =
            pushRow (slice 6 9 gs) 0
            
    in
    (append row1 (append row2 row3), (sc1 + sc2 + sc3))


moveRight : Array Int -> (Array Int, Int)
moveRight gs =
    let
        (row1, sc1) =
            pushRow (fromList (reverse (toList (slice 0 3 gs)))) 0

        (row2, sc2) =
            pushRow (fromList (reverse (toList (slice 3 6 gs)))) 0

        (row3, sc3) =
            pushRow (fromList (reverse (toList (slice 6 9 gs)))) 0
    in
    (append (fromList (reverse (toList (row1)))) (append (fromList (reverse (toList (row2)))) (fromList (reverse (toList (row3))))), (sc1 + sc2 + sc3))


moveUp : Array Int -> (Array Int, Int)
moveUp gs =
    let
        row1 =
            fromList [ getInt 0 gs, getInt 3 gs, getInt 6 gs ]

        row2 =
            fromList [ getInt 1 gs, getInt 4 gs, getInt 7 gs ]

        row3 =
            fromList [ getInt 2 gs, getInt 5 gs, getInt 8 gs ]

        (result, sc) =
            moveLeft (append row1 (append row2 row3))
    in
    (fromList [ getInt 0 result, getInt 3 result, getInt 6 result, getInt 1 result, getInt 4 result, getInt 7 result, getInt 2 result, getInt 5 result, getInt 8 result ], sc)


moveDown : Array Int -> (Array Int, Int)
moveDown gs =
    let
        row1 =
            fromList [ getInt 0 gs, getInt 3 gs, getInt 6 gs ]

        row2 =
            fromList [ getInt 1 gs, getInt 4 gs, getInt 7 gs ]

        row3 =
            fromList [ getInt 2 gs, getInt 5 gs, getInt 8 gs ]

        (result, sc) =
            moveRight (append row1 (append row2 row3))
    in
    (fromList [ getInt 0 result, getInt 3 result, getInt 6 result, getInt 1 result, getInt 4 result, getInt 7 result, getInt 2 result, getInt 5 result, getInt 8 result ], sc)


pushRow : Array Int -> Int -> (Array Int, Int)
pushRow row sc =
    let
        x1 =
            getInt 0 row

        x2 =
            getInt 1 row

        x3 =
            getInt 2 row
    in
    if x1 + x2 + x3 == 0 then
        (row, sc)

    else if x1 == 0 then
        pushRow (append (fromList [ x2 ]) (fromList [ x3, 0 ])) sc

    else if x1 == x2 then
        pushRow (append (repeat 1 (x1 + x2)) (fromList [ x3, 0 ])) (sc + x1 + x2)
        

    else if x2 == x3 then
        (append (fromList [ x1 ]) (fromList [ x2 + x3, 0 ]), (sc + x2 + x3))

    else
        (row, sc)


-- SUBS

subs : Model -> Sub Msg
subs model =
    Sub.none

-- VIEW


view : Model -> Html Msg
view model =
    div [ class "game" ]
        [ div [ class "rows" ]
            [ div [ class "row row1" ]
                [ span [ classList [ ("number", not(getInt 0 model.gameState == 0)) ] ] [ text (String.fromInt (getInt 0 model.gameState)) ]
                , span [ classList [ ("number", not(getInt 1 model.gameState == 0)) ] ] [ text (String.fromInt (getInt 1 model.gameState)) ]
                , span [ classList [ ("number", not(getInt 2 model.gameState == 0)) ] ] [ text (String.fromInt (getInt 2 model.gameState)) ]
                ]
            , div [ class "row row2" ]
                [ span [ classList [ ("number", not(getInt 3 model.gameState == 0)) ] ] [ text (String.fromInt (getInt 3 model.gameState)) ]
                , span [ classList [ ("number", not(getInt 4 model.gameState == 0)) ] ] [ text (String.fromInt (getInt 4 model.gameState)) ]
                , span [ classList [ ("number", not(getInt 5 model.gameState == 0)) ] ] [ text (String.fromInt (getInt 5 model.gameState)) ]
                ]
            , div [ class "row row3" ]
                [ span [ classList [ ("number", not(getInt 6 model.gameState == 0)) ] ] [ text (String.fromInt (getInt 6 model.gameState)) ]
                , span [ classList [ ("number", not(getInt 7 model.gameState == 0)) ] ] [ text (String.fromInt (getInt 7 model.gameState)) ]
                , span [ classList [ ("number", not(getInt 8 model.gameState == 0)) ] ] [ text (String.fromInt (getInt 8 model.gameState)) ]
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
            , div [][text (String.fromInt model.score)]
        ]



-- PROGRAM

getInt : Int -> Array Int -> Int
getInt pos arr =
    case get pos arr of
        Just a ->
            a

        Nothing ->
            -1
