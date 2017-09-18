module MarkerUtils exposing (cluster, flatten_map, points_to_string)

import Map exposing (..)


{- If a point is not within cluster distance of anything,
   add it to the final Map.
   If a point is within cluster distance, add it and 
   the other points within the distance to a cluster 
   and add the Cluster to the Map.
   Cluster the excluded points.

   Note - this greedily clusters. This is not 'optimal'.
-}
cluster : List PointXY -> Float -> Map -> Map
cluster points cluster_distance acc = 
    let 
        split_points = \p -> 
            List.partition (\a -> (distance a p) < cluster_distance) points
        add_logic = \pts existing ->
            if List.length pts < 2 then
                case List.head pts of
                    Just p ->
                        (Point p "") :: existing
                    Nothing ->
                        existing
            else
                (Cluster (Clustering (average pts) pts)) :: existing
    in
        case List.head points of
            Just p ->
                let 
                    (close_points, distant_points) = (split_points p)
                    new_map = (add_logic close_points acc)
                in
                    (cluster distant_points cluster_distance new_map)
            Nothing ->
                acc



distance : PointXY -> PointXY -> Float
distance p1 p2 =
    let 
        x1 = p1.x
        y1 = p1.y
        x2 = p2.x
        y2 = p2.y
    in
        (sqrt ((x1 - x2)^2 + (y1 - y2)^2))


average : List PointXY -> PointXY
average points = 
    let 
        length = (toFloat (List.length points))
    in
        PointXY ((List.sum (List.map (\p -> p.x) points)) / length)
                ((List.sum (List.map (\p -> p.y) points)) / length)

flatten_map : Map -> List PointXY
flatten_map map =
    List.foldr
        (\item acc -> 
            case item of
                Point e s ->
                    e :: acc
                Cluster c ->
                    List.append c.points acc
        )
        []
        map


points_to_string : List PointXY -> String
points_to_string ps = 
    List.foldr
        (\item acc ->
            acc ++ "(" ++ (toString item.x) ++ ", " ++ (toString item.y) ++ ") "
        )
        ""
        ps