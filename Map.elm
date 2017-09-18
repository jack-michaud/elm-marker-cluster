module Map exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Svg exposing (circle, line, svg)
import Svg.Attributes exposing (cx, cy, r, fill, viewBox, width)


type alias PointXY = 
    { x : Float
    , y : Float 
    }

type alias Clustering =
    { center : PointXY
    , points : List PointXY
    }

type MapMarker = Point PointXY String | Cluster Clustering
type alias Map = List MapMarker


map_width = 
    300
map_height = 
    300

map_center = 
    PointXY (map_width / 2.0) (map_height / 2.0) 

render_map : Map -> Html msg
render_map map =
    svg [ viewBox ("0 0 " ++ toString map_width ++ " " ++ toString map_height), width (toString map_width) ]
          (List.map marker_render map)
        

marker_render : MapMarker -> Svg.Svg msg
marker_render mm =
    case mm of 
        Point p s -> 
            make_circle p.x p.y 10
        Cluster c ->
            make_circle c.center.x c.center.y 20

            
make_circle : Float -> Float -> Float -> Svg.Svg msg
make_circle x y radius =
    circle [ cx (toString x), cy (toString y), r (toString radius), fill "#0B79CE" ] []      

my_map = 
    [ (PointXY 50.0 25.0)
    , (PointXY 2.0 2.0)
    , (PointXY 100 100)
    , (PointXY 150 150) 
    , (PointXY 160 150) 
    , (PointXY 140 150)
    ]
