module Main exposing (..)


import Html exposing (Html, div, text, input, li, ul)
import Html.Attributes exposing (style, type_, placeholder)
import Html.Events exposing (onInput)

import MarkerUtils exposing (cluster, flatten_map, points_to_string)
import Map exposing (render_map, my_map, Map, PointXY)
--import Widgets exposing (pointsWidget, addPointWidget)


type alias Model =
    { points : List PointXY
    , radius : Float }

init_model : Model
init_model = 
    Model my_map 50


type Msg = None | AdjustRadius String | AddPoint String String

main =
    Html.beginnerProgram { model = init_model, update = update, view = view}

view : Model -> Html Msg
view model =
    let 
        clustered_map = (cluster model.points model.radius [])
    in
      div [ global_style ]
        [ render_map clustered_map
        , adjustRadiusWidget
        , pointsWidget model.points
        ]

update : Msg -> Model -> Model
update msg model =
    case msg of
        None -> 
            model
        AdjustRadius i ->
            { model | radius = (
                        (\r ->
                            case String.toFloat r of 
                                Ok f ->
                                    f
                                Err e ->
                                    model.radius) 
                        i)}
        AddPoint x y ->
            case String.toFloat x of
                Ok f1 ->
                    case String.toFloat y of
                        Ok f2 ->
                            { model | points = (PointXY f1 f2) :: model.points }
                        Err e ->
                            model
                Err e ->
                    model


global_style = 
    style [
      ("backgroundColor","black")
    , ("height", "100vh")
    , ("color", "white")
    
    ]

pointsWidget : List PointXY -> Html Msg
pointsWidget pts = 
    text ( pts |> points_to_string) 

--adjustRadiusWidget : _ -> Html Msg
adjustRadiusWidget = 
    input [ type_ "number", placeholder "50", onInput AdjustRadius ] []