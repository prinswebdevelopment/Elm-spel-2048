module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, span, text, input)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Helper



-- MAIN


main : Program () Model Msg
main =
  Browser.element { init = init, update = update, view = view, subscriptions = subs }



-- MODEL

type alias Model = 
  { game: Int
  , message: String
  , gamePlay: List Int
  }


init : flags -> (Model, Cmd Msg)
init flags =
  ({ game = 0
  , message = ""
  , gamePlay = [0,2,0,2,8,8,8,16,0,8,8,0,0,2,0,8]
  }, Cmd.none )



-- UPDATE


type Msg
  = Reset
  | Change String
  | Left
  | Right
  | Down
  | Up
  | Random
  | Generated Int
  

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Random -> (updateHelper msg model, Random.generate (\x -> Generated x) (Random.int 1 100))
    other -> (updateHelper msg model, Cmd.none)


updateHelper : Msg -> Model -> Model
updateHelper msg model =
  case msg of

    Reset ->
      { model | gamePlay = [] }

    Change text ->
      { model | message = text }

    Left ->
      { model | gamePlay = Helper.moveLeft model.gamePlay }

    Right ->
      { model | gamePlay = Helper.moveRight model.gamePlay }

    Down ->
      { model | gamePlay = Helper.moveDown model.gamePlay }

    Up ->
      { model | gamePlay = Helper.moveUp model.gamePlay }

    Random ->
      { model | message =  String.fromInt model.game }

    Generated new ->
      { model | game = new }

-- SUBS

subs : Model -> Sub Msg
subs model =
    Sub.none


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ div[] [ text ("2048") ]
    , button [ onClick Reset ] [ text "Reset" ]
    , button [ onClick Left ] [ text "Left" ]
    , button [ onClick Right ] [ text "Right" ]
    , button [ onClick Down ] [ text "Down" ]
    , button [ onClick Up ] [ text "Up" ]
    , button [ onClick Random ] [ text "Random" ]
    , div[] [text (Helper.gameOutput model.gamePlay)]
    , div[] [text ("split: " ++ Debug.toString (Helper.split model.gamePlay))]
    , div[] [text ("splitCol: " ++ Debug.toString (Helper.splitCol model.gamePlay))]
    , div[] [text ("getCol: " ++ Debug.toString (Helper.getCol 3 model.gamePlay))]
    , div[] [text ("getRow: " ++ Debug.toString (Helper.getRow model.gamePlay 1))]
    , div[] [text ("clearZeros: " ++ Debug.toString (Helper.clearZeros [0,2,2,2]))]
    , div[] [text ("combine: " ++ Debug.toString (Helper.combine [2,4,8]))]
    , div[] [text ("grow: " ++ Debug.toString (Helper.grow 2 [4]))]
    , div[] [text ("moveLeft: " ++ Debug.toString (Helper.moveLeft model.gamePlay))]
    , div[] [text ("moveRight: " ++ Debug.toString (Helper.moveRight model.gamePlay))]
    , div[] [text ("moveDown: " ++ Debug.toString (Helper.moveDown model.gamePlay))]
    , div[] [text ("moveUp: " ++ Debug.toString (Helper.moveUp model.gamePlay))]
    , div[] [text ("var game: " ++ Debug.toString model.game)]
    , div[] [text ("var message: " ++ Debug.toString model.message)]
    , div[] [text "tekst"]
    , viewHelper model.gamePlay
    ]

viewHelper : List Int -> Html msg
viewHelper g =
    div [] (List.map viewRow (Helper.split g))
    
viewRow : List Int -> Html msg
viewRow r =
    div [ class "row" ] (List.map viewItem r)

viewItem : Int -> Html msg
viewItem x =
    span [ class "item"] [ text (String.fromInt x) ]
  