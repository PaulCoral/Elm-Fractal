module Main exposing (main)

import Browser
import Canvas.Settings.Line exposing (lineWidth)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Canvas exposing (shapes)
import Canvas.Settings exposing (fill,stroke)
import Canvas.Settings.Advanced exposing (transform, Transform, scale, translate)
import Canvas.Settings.Line exposing (lineWidth)
import Color
import Time exposing (..)
import Json.Decode as Json
import Random exposing (..)

import FracPattern exposing (..)
import Drawable exposing (..)
import PointSpace
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
  , transform : ModelTransform
  }


{-| Record of the live changes in the form
-}
type alias ModelForm =
    { pattern : String
    , counterIsSet : Bool
    }


type alias ModelTransform =
    { scale : Float
    , translate : PointSpace.Point
    , prevPos : PointSpace.Point
    , mouseClicked : Bool
    }

type Zoom = In | Out

zoomFactor : Float
zoomFactor = 1


{-| initialization function
-}
init :() -> (Model, Cmd Msg)
init _ =
    (Model
        0
        initModelForm
        initDrawingState
        initCounter
        initModelTransform
    , Cmd.none
    )


defaultFormPatternText : String
defaultFormPatternText = ""


initModelForm : ModelForm
initModelForm = (ModelForm defaultFormPatternText initCounter.isEnabled)


initModelTransform : ModelTransform
initModelTransform =
    ModelTransform
        (1)
        (PointSpace.Point 0 0)
        (PointSpace.Point 0 0)
        (False)


modelTransformToList : ModelTransform -> List Transform
modelTransformToList mt =
    let
        s = mt.scale
        x = mt.translate.x / s
        y = mt.translate.y / s
    in
        [ scale s s
        , translate x y
        ]

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
    | NextIter
    | Reset
    | UpdateForm ModelForm
    | Scale Zoom
    | Translate Float Float
    | MouseClick Bool Float Float
    | GeneratePresets
    | RandomPresets Angles
    | None


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

        Scale zoom ->
            ( updateModelScale zoom model
            , Cmd.none
            )

        Translate x y ->
            ( updateModelTranslate x y model
            , Cmd.none
            )

        MouseClick clicked x y ->
            ( updateMouseClick
                clicked
                (PointSpace.Point x y)
                model
            , Cmd.none
            )

        GeneratePresets ->
            ( model
            , Random.generate RandomPresets (Random.list 6 (Random.float -180 180))
            )

        RandomPresets ls ->
            let
                trunc = (List.map (\f -> ( toFloat ( truncate f ) ) ) ls )
                modelform = model.form
                newModelform = { modelform | pattern = (anglesToString trunc) }
                newModel =  { model | form = newModelform }
            in
            (newModel, Cmd.none)

        None -> (model, Cmd.none)



updateMouseClick : Bool -> PointSpace.Point -> Model -> Model
updateMouseClick clicked point model =
    let
        transform = model.transform
        prevPos = transform.prevPos
    in
        { model
        | transform =
            { transform
            | mouseClicked = clicked
            , prevPos = if clicked then point else prevPos
            }
        }


updateModelScale : Zoom -> Model -> Model
updateModelScale zoom model =
    let
        modeltrans = model.transform
        prevScale = model.transform.scale
        factor =
            case zoom of
                In -> zoomFactor
                Out -> (negate zoomFactor)
    in
        { model
        | transform =
            { modeltrans
            | scale = prevScale + factor
            }
        }



updateModelTranslate : Float -> Float -> Model -> Model
updateModelTranslate newX newY model =
    let
        modeltrans = model.transform
        translate = (modeltrans.translate)
        prevPos = (modeltrans.prevPos)
        updatedX = translate.x + newX - prevPos.x
        updatedY = translate.y + newY - prevPos.y
        updatedModel =
            { model
            | transform =
                { modeltrans
                | translate = ( PointSpace.Point updatedX updatedY )
                , prevPos = PointSpace.Point newX newY
                }
            }
    in
        if model.transform.mouseClicked then
            updatedModel
        else
            model




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
            , button [ onClick GeneratePresets ] [ text "Random" ]
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
       options =
           (option [ value form.pattern ] [text "Custom"])
               :: (List.map
                       (\preset ->
                           option [ value preset.pattern ] [text preset.name]
                       )
                       (presetList))
    in
        select
            [ onInput (\s ->( UpdateForm ( { form | pattern = s } ) ) ) ]
            options


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
        , br [] []
        , text "Zoom : "
        , button [ onClick (Scale In) ] [ text "+" ]
        , button [ onClick (Scale Out) ] [text "-"]
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
            [ onMouseMove Translate
            , onMyMouseUp (MouseClick False)
            , onMyMouseDown (MouseClick True)
            , onMouseOut (MouseClick False 0 0)
            ]
            [ shapes
                [ fill Color.black ]
                [ Canvas.rect (0,0) (toFloat size) (toFloat size) ]
            , shapes
                [ fill Color.black
                , stroke Color.white
                , transform (modelTransformToList model.transform)
                , lineWidth (1 / model.transform.scale)
                ]
                ( linesToShape ( model.drawing.lines ) )
            ]

onMyMouseDown : (Float -> Float -> msg) -> Attribute msg
onMyMouseDown f =
    on "mousedown" (mouseMoveDecoder f)


onMyMouseUp : (Float -> Float -> msg) -> Attribute msg
onMyMouseUp f =
    on "mouseup" (mouseMoveDecoder f)


onMouseMove : (Float -> Float -> msg) -> Attribute msg
onMouseMove f =
    on "mousemove" (mouseMoveDecoder f)


mouseMoveDecoder : (Float -> Float -> msg) -> Json.Decoder msg
mouseMoveDecoder f =
    Json.map2 f
        (Json.field "offsetX" Json.float)
        (Json.field "offsetY" Json.float)


onKeyPressed : (Int -> msg) -> Attribute msg
onKeyPressed func =
    on "keypress" (Json.map func keyCode)