module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import FracPattern exposing (..)

-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { nbIterations : Int
  , pattern : FracPattern
  , form : ModelForm
  }

type alias ModelForm =
    { pattern : String
    , nbIter : Int
    }

init : Model
init = Model 0 [] (ModelForm "" 0)

isInitModel : Model -> Bool
isInitModel model =
    model.pattern == emptyFracPattern

type Msg
    = Draw
    | AddIter
    | RemoveIter
    | Reset
    | UpdateForm ModelForm

update : Msg -> Model -> Model
update msg model =
    case msg of
        Draw ->
            { model
            | nbIterations = model.form.nbIter
            , pattern = (fracPatternFromString model.form.pattern)
            }
        AddIter -> {model | nbIterations = model.nbIterations + 1}
        RemoveIter -> {model | nbIterations = model.nbIterations - 1}
        Reset -> init
        UpdateForm mf -> {model | form = mf}

view : Model -> Html Msg
view model =
    div [] [ viewCommand model ]


viewCommand : Model -> Html Msg
viewCommand model =
    if isInitModel model then
        viewCommandInit model
    else
        viewCommandUpdate model

viewCommandInit : Model -> Html Msg
viewCommandInit model =
    div []
        [ input [ type_ "text", onInput (patFromText model) ] []
        , input [ type_ "number", onInput (nbIterFromText model)] []
        , br [] []
        , button [onClick Draw] [text "Enter"]
        , button [ onClick Reset ] [text "Reset"]
        ]

viewCommandUpdate : Model -> Html Msg
viewCommandUpdate model =
    div []
        [ text ("Pattern : " ++ (fracPatternToString model.pattern))
        , br [] []
        , text ("Number of iterations : " ++ (String.fromInt model.nbIterations))
        , br [] []
        , button [ onClick AddIter ] [ text "+" ]
        , br [] []
        , button [ onClick RemoveIter ] [ text "-" ]
        ]

patFromText : Model -> String -> Msg
patFromText model s =
    let
        prevForm = model.form
    in
        UpdateForm { prevForm | pattern = s}

nbIterFromText : Model -> String -> Msg
nbIterFromText model s =
    let
        prevForm = model.form
        nb =
            case (String.toInt s) of
                Nothing -> prevForm.nbIter
                Just a -> a
    in
        UpdateForm { prevForm | nbIter = nb}








