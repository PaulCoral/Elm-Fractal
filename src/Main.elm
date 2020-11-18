module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Canvas exposing (shapes)
import Canvas.Settings exposing (fill,stroke)
import Color
import Time exposing (..)

import FracPattern exposing (..)
import Drawable exposing (..)
import PresetPattern exposing (presetList)
import Counter exposing (..)



-- MAIN

{-| the main function
-}
main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


-- MODEL


{-| The application model
-}
type alias Model =
  { nbIterations : Int
  , form : ModelForm
  , drawing : DrawingState
  , counter : Counter
  }


{-| Record of the live changes in the form
-}
type alias ModelForm =
    { pattern : String
    , counterIsSet : Bool
    }


{-| initialization function
-}
init :() -> (Model, Cmd Msg)
init _ =
    (Model
        1
        initModelForm
        initDrawingState
        initCounter
    , Cmd.none
    )


defaultFormPatternText : String
defaultFormPatternText = ""


initModelForm : ModelForm
initModelForm = (ModelForm defaultFormPatternText initCounter.isEnabled)

{-| Return true if the model is the one at the application start
-}
isStartModel : Model -> Bool
isStartModel model =
    model.drawing.pattern == emptyAngles


nextDrawingIteration : Model -> DrawingState
nextDrawingIteration model =
    updateDrawingState model.drawing


-- MSG and UPDATE


{-| Messages for application update
-}
type Msg
    = Draw
    --| Tick Time.Posix
    | NextIter
    | Reset
    | UpdateForm ModelForm


{-| Update the application Model from a Msg and the current Model
-}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Draw ->
            ({ model
            | drawing =
                addPatternToDrawingState
                    (model.drawing)
                    (anglesFromString model.form.pattern)
            , counter = setCounter model.counter model.form.counterIsSet
            }
            , Cmd.none
            )
        NextIter ->
            ({ model
            | nbIterations = model.nbIterations + 1
            , drawing = nextDrawingIteration model
            }
            , Cmd.none
            )
        Reset ->
            let
                (m, cmd) = init ()
                newModel = { m | form = model.form}
            in
                (newModel, cmd)

        UpdateForm mf -> ({model | form = mf}, Cmd.none)



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    let
         c = model.counter
         i = model.nbIterations
         hasNext = (c.isEnabled && (i <= c.upTo))
    in
        if hasNext then
            Time.every Counter.timeInterval (\_ -> NextIter)
        else
            Sub.none



-- VIEW


{-| The view of the application. Created from a Model
-}
view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "width" "100%"
        , style "font-family" "sans-serif"
        ]
        [ viewText model
        , viewDrawing model
        ]

viewText : Model -> Html Msg
viewText model =
    div
        [ style "margin" "3em" ]
        [ h1
            []
            [ text "Elm Fractal" ]
        , p
            []
            [ text "Have a look at my "
            , a [ href "https://github.com/lepaincestbon/Elm-Fractal" ] [ text "GitHub Repo" ]
            ]
        , viewCommand model
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
    let
        form = model.form
    in
        div []
            [ input
                [ type_ "text"
                , value form.pattern
                , onInput (updatePatternModelForm model)
                ]
                []
            , br [] []
            , viewCommandPreset model
            , br [] []
            , text "Animated ?"
            , input
                [ type_ "checkbox"
                , checked model.form.counterIsSet
                , onCheck
                    (\bool ->
                        (UpdateForm {form | counterIsSet = bool}))
                ]
                []
            , br [] []
            , button [onClick Draw] [text "Enter"]
            , button [ onClick Reset ] [text "Reset"]
            ]


{-| Show a list of presets
-}
viewCommandPreset : Model -> Html Msg
viewCommandPreset model =
    let
       form = model.form
    in
        select [ onInput (\s ->( UpdateForm ({form | pattern = s }))) ]
            (  (option [ value form.pattern ] [text "Custom"])
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
        , text ("Number of drawn lines :" ++ (String.fromInt (List.length (model.drawing.lines))))
        , br [] []
        , button [ onClick NextIter] [ text "Next" ]
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
        size = round initLineLength
    in
        Canvas.toHtml
            (size, size)
            []
            [ shapes
                [ fill Color.black ]
                [ Canvas.rect (0,0) (toFloat size) (toFloat size) ]
            , shapes
                [ fill Color.black
                , stroke Color.white
                ]
                ( linesToShape ( model.drawing.lines ) )
            ]
