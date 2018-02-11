module Main exposing (main)

import Html exposing (Html, text, div, button, hr, span)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import Array exposing (..)
import List exposing (..)

main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initalModel
        , update = update
        , view = view
        }



-- Model


type alias Model =
    { value : Int
    , counterIndexes : List Int
    , counterValues : Array Int
    , currentCounter : Int
    }

-- maximum of counters starting from 0
maxCounter : Int
maxCounter = 2

counters : Array Int
counters = Array.repeat (maxCounter + 1) 0

initalModel : Model
initalModel =
    { value = 0
    , counterIndexes = List.range 0 maxCounter
    , counterValues = counters
    , currentCounter = 0
    }



-- Update


type Msg
    = Increment
    | Decrement
    | Increment5
    | Decrement5
    | Inc
    | Dec


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | value = model.value + 1 }
        Increment5 ->
            { model | value = model.value + 5 }

        Decrement ->
            { model | value = model.value - 1 }
        Decrement5 ->
            { model | value = model.value - 5 }

        Inc ->
            { model | counterValues = (incElem model.counterValues model.currentCounter) }
        Dec ->
            { model | counterValues = (decElem model.counterValues model.currentCounter) }


-- TODO: finish those two functions
incElem vs c =
    get c vs |> (\v -> set c (v+1) vs)


decElem vs c =
    vs

-- View


viewCounter model current =
  div[] [ button [ onClick Inc] [text "+1"]
        , span   [] [text (toString (get current model.counterValues))]
        , button [ onClick Dec] [text "-1"]
        ]

view : Model -> Html Msg
view model =
    div []
        [ div [ class "counter" ]
              [ text (toString model.value) ]
        , div [ class "controls" ]
            [ button [ onClick Increment5 ] [ text "+5" ]
            , button [ onClick Increment ]  [ text "+1" ]
            , button [ onClick Decrement ]  [ text "-1" ]
            , button [ onClick Decrement5 ] [ text "-5" ]
            ]

        , hr [] []
        , div [] (List.map (\x -> viewCounter model x ) model.counterIndexes  )
        ]
