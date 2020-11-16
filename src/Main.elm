module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Svg exposing (svg)
import Svg.Attributes

import FracPattern exposing (..)
import Drawable exposing (..)
import PresetPattern exposing (presetList)



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
  , form : ModelForm
  , drawing : DrawingState
  }


{-| Record of the live changes in the form
-}
type alias ModelForm = { pattern : String }


{-| initialization function
-}
init : Model
init =
    Model
        1
        initModelForm
        initDrawingState


defaultFormPatternText : String
defaultFormPatternText = ""


initModelForm : ModelForm
initModelForm = (ModelForm defaultFormPatternText)

{-| Return true if the model is the one at the application start
-}
isStartModel : Model -> Bool
isStartModel model =
    model.drawing.pattern == emptyAngles


nextDrawingIteration : Model -> DrawingState
nextDrawingIteration model =
    updateDrawingState model.drawing


modelReset : Model -> Model
modelReset model =
    { model
    | nbIterations = init.nbIterations
    , drawing = init.drawing
    }


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
            | drawing =
                addPatternToDrawingState
                    (model.drawing)
                    (anglesFromString model.form.pattern)
            }
        NextIter ->
            { model
            | nbIterations = model.nbIterations + 1
            , drawing = nextDrawingIteration model
            }
        Reset -> modelReset model
        UpdateForm mf -> {model | form = mf}


{-| The view of the application. Created from a Model
-}
view : Model -> Html Msg
view model =
    div
        []
        [ h1 [] [text "Elm Fractal"]
        , p []
            [ text "Have a look at my "
            , a [ href "https://github.com/lepaincestbon/Elm-Fractal" ] [ text "GitHub Repo" ]
            ]
        , viewCommand model
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
        [ input [ type_ "text", value model.form.pattern ,onInput (updatePatternModelForm model) ] []
        , br [] []
        , viewCommandPreset model
        , br [] []
        , button [onClick Draw] [text "Enter"]
        , button [ onClick Reset ] [text "Reset"]
        ]


{-| Show a list of presets
-}
viewCommandPreset : Model -> Html Msg
viewCommandPreset model =
    select [ onInput (\s -> UpdateForm (ModelForm s)) ]
        (  (option [ value model.form.pattern ] [text "Custom"])
        :: (List.map
            (\preset ->
                option [ value preset.pattern ] [text preset.name]
            )
            presetList))


{-| The UI to update application state
-}
viewCommandUpdate : Model -> Html Msg
viewCommandUpdate model =
    div []
        [ text ("Pattern : " ++ (anglesToString model.drawing.pattern))
        , br [] []
        , text ("Number of iterations : " ++ (String.fromInt model.nbIterations))
        , br [] []
        , text ("Number of drawn lines :" ++ (String.fromInt (List.length (linesToSvgLines (model.drawing.lines)))))
        , br [] []
        , button [ onClick NextIter ] [ text "Next" ]
        , button [ onClick Reset ] [text "Reset"]
        ]


{-| Create Msg to update `ModelForm.pattern` from String
-}
updatePatternModelForm : Model -> String -> Msg
updatePatternModelForm model s =
    let
        prevForm = model.form
    in
        UpdateForm { prevForm | pattern = s}


{-| The view with the svg drawing of the fractal
-}
viewDrawing : Model -> Html Msg
viewDrawing model =
    let
        sizeString = String.fromFloat initLineLength
    in
    svg
        [ Svg.Attributes.viewBox ("0 0 " ++ sizeString ++ " " ++ sizeString)
        , Svg.Attributes.width sizeString
        , Svg.Attributes.height sizeString
        ]
        (linesToSvgLines (model.drawing.lines))

