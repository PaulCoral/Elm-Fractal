module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Svg exposing (..)
import Svg.Attributes exposing (..)

import FracPattern exposing (..)
import Drawable exposing (..)



-- MAIN

{-| the main function
-}
main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL


{-| The application model
-}
type alias Model =
  { nbIterations : Int
  , pattern : FracPattern
  , form : ModelForm
  , drawing : DrawingState
  }


{-| Record of the live changes in the form
-}
type alias ModelForm =
    { pattern : String
    , nbIter : Int
    }


{-| initialization function
-}
init : Model
init =
    Model
        0
        []
        initModelForm
        initDrawingState


initModelForm : ModelForm
initModelForm = (ModelForm "" 0)

{-| Return true if the model is the one at the application start
-}
isStartModel : Model -> Bool
isStartModel model =
    model.pattern == emptyFracPattern


newDrawingStateFromModel : Model -> DrawingState
newDrawingStateFromModel model =
    let
        ds = model.drawing
        updatePattern =
            addPatternToDrawingState
                ds
                model.pattern
    in
        updateDrawingState
            updatePattern
            model.nbIterations



{-| Messages for application update
-}
type Msg
    = Draw
    | NextIter
    | Reset
    | UpdateForm ModelForm


{-| Update the application Model from a Msg and the current Model
-}
update : Msg -> Model -> Model
update msg model =
    case msg of
        Draw ->
            { model
            | nbIterations = model.form.nbIter
            , pattern = (fracPatternFromString model.form.pattern)
            , drawing = newDrawingStateFromModel model
            }
        NextIter ->
            { model
            | nbIterations = model.nbIterations + 1
            , drawing = newDrawingStateFromModel model
            }
        Reset -> init
        UpdateForm mf -> {model | form = mf}


{-| The view of the application. Created from a Model
-}
view : Model -> Html Msg
view model =
    div
        []
        [ viewCommand model
        , viewDrawing model
        ]


{-| Return the appropriate UI to display
-}
viewCommand : Model -> Html Msg
viewCommand model =
    if isStartModel model then
        viewCommandInit model
    else
        viewCommandUpdate model


{-| The UI at application start.
-}
viewCommandInit : Model -> Html Msg
viewCommandInit model =
    div []
        [ input [ type_ "text", onInput (updatePatternModelForm model) ] []
        , input [ type_ "number", onInput (updateNbIterModelForm model)] []
        , br [] []
        , button [onClick Draw] [text "Enter"]
        , button [ onClick Reset ] [text "Reset"]
        ]


{-| The UI to update application state
-}
viewCommandUpdate : Model -> Html Msg
viewCommandUpdate model =
    div []
        [ text ("Pattern : " ++ (fracPatternToString model.pattern))
        , br [] []
        , text ("Number of iterations : " ++ (String.fromInt model.nbIterations))
        , br [] []
        , button [ onClick NextIter ] [ text "Next" ]
        ]


{-| Create Msg to update `ModelForm.pattern` from String
-}
updatePatternModelForm : Model -> String -> Msg
updatePatternModelForm model s =
    let
        prevForm = model.form
    in
        UpdateForm { prevForm | pattern = s}


{-| Create Msg to update `ModelForm.nbIter` from String
-}
updateNbIterModelForm : Model -> String -> Msg
updateNbIterModelForm model s =
    let
        prevForm = model.form
        nb =
            case (String.toInt s) of
                Nothing -> prevForm.nbIter
                Just a -> a
    in
        UpdateForm { prevForm | nbIter = nb}


viewDrawing : Model -> Html Msg
viewDrawing model =
    svg
        [ viewBox "0 0 400 400"
        , Svg.Attributes.width "400"
        , Svg.Attributes.width "400"
        ]
        (linesToSvgLines (model.drawing.lines))
