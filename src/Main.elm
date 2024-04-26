module Main exposing (main)

import Array exposing (..)
import Browser
import Html exposing (Attribute, Html, button, div, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (reverse)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { gameState : Array Int
    }


init : Model
init =
    { gameState = fromList [ 0, 0, 2, 0, 2, 4, 4, 0, 4 ] }



-- UPDATE


type Msg
    = Left
    | Right
    | Up
    | Down


update : Msg -> Model -> Model
update msg model =
    case msg of
        Left ->
            { model | gameState = moveLeft model.gameState }

        Right ->
            { model | gameState = moveRight model.gameState }

        Up ->
            { model | gameState = moveUp model.gameState }

        Down ->
            { model | gameState = moveDown model.gameState }


moveLeft : Array Int -> Array Int
moveLeft gs =
    let
        row1 =
            slice 0 3 gs

        row2 =
            slice 3 6 gs

        row3 =
            slice 6 9 gs
    in
    append (pushRow row1) (append (pushRow row2) (pushRow row3))


moveRight : Array Int -> Array Int
moveRight gs =
    let
        row1 =
            fromList (reverse (toList (slice 0 3 gs)))

        row2 =
            fromList (reverse (toList (slice 3 6 gs)))

        row3 =
            fromList (reverse (toList (slice 6 9 gs)))
    in
    append (fromList (reverse (toList (pushRow row1)))) (append (fromList (reverse (toList (pushRow row2)))) (fromList (reverse (toList (pushRow row3)))))


moveUp : Array Int -> Array Int
moveUp gs =
    let
        row1 =
            fromList [ getInt 0 gs, getInt 3 gs, getInt 6 gs ]

        row2 =
            fromList [ getInt 1 gs, getInt 4 gs, getInt 7 gs ]

        row3 =
            fromList [ getInt 2 gs, getInt 5 gs, getInt 8 gs ]

        result =
            moveLeft (append row1 (append row2 row3))
    in
    fromList [ getInt 0 result, getInt 3 result, getInt 6 result, getInt 1 result, getInt 4 result, getInt 7 result, getInt 2 result, getInt 5 result, getInt 8 result ]


moveDown : Array Int -> Array Int
moveDown gs =
    let
        row1 =
            fromList [ getInt 0 gs, getInt 3 gs, getInt 6 gs ]

        row2 =
            fromList [ getInt 1 gs, getInt 4 gs, getInt 7 gs ]

        row3 =
            fromList [ getInt 2 gs, getInt 5 gs, getInt 8 gs ]

        result =
            moveRight (append row1 (append row2 row3))
    in
    fromList [ getInt 0 result, getInt 3 result, getInt 6 result, getInt 1 result, getInt 4 result, getInt 7 result, getInt 2 result, getInt 5 result, getInt 8 result ]


pushRow : Array Int -> Array Int
pushRow row =
    let
        x1 =
            getInt 0 row

        x2 =
            getInt 1 row

        x3 =
            getInt 2 row
    in
    if x1 + x2 + x3 == 0 then
        row

    else if x1 == 0 then
        pushRow (append (fromList [ x2 ]) (fromList [ x3, 0 ]))

    else if x1 == x2 then
        pushRow (append (repeat 1 (x1 + x2)) (fromList [ x3, 0 ]))

    else if x2 == x3 then
        append (fromList [ x1 ]) (fromList [ x2 + x3, 0 ])

    else
        row



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
        ]



-- PROGRAM

getInt : Int -> Array Int -> Int
getInt pos arr =
    case get pos arr of
        Just a ->
            a

        Nothing ->
            -1
