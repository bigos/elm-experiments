module Main exposing (main)

import Html exposing (Html, text, div, button, hr, span)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import Array exposing (Array, repeat, get, set)
import List exposing (map, range)

main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initalModel
        , update = update
        , view = view
        }



-- Model


maxCounter : Int
maxCounter = 3

type alias Model =
    { value : Int
    , counterValues : Array Int
    }

initalModel : Model
initalModel =
    { value = 0
    , counterValues = Array.repeat (maxCounter + 1) 0
    }



-- Update


type Msg
    = Increment
    | Decrement
    | Increment5
    | Decrement5
    | Inc Int
    | Dec Int


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

        Inc current ->
            { model | counterValues = (incDecElem
                                           (+)
                                           model.counterValues
                                           current) }
        Dec current ->
            { model | counterValues = (incDecElem
                                           (-)
                                           model.counterValues
                                           current) }


incDecElem : (a -> number -> a) -> Array a -> Int -> Array a
incDecElem fn vs c =
    let
        v = get c vs
    in
        case v of
            Nothing ->
                vs
            Just v ->
                set c (fn v 1) vs

toStr : Maybe a -> String
toStr v =
    case v of
        Nothing ->
            ""
        Just v ->
            toString v

-- View

viewCounters : { b | counterValues : Array a } -> Int -> Html Msg
viewCounters model current =
    div[] [ button [ onClick (Inc current)] [text "+1"]
        , span   [] [text (toStr (get current model.counterValues))]
          , button [ onClick (Dec current)] [text "-1"]
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
        , div [] (List.map (\x -> viewCounters model x ) (range 1 maxCounter)  )
        ]
