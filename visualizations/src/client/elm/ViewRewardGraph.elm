module ViewRewardGraph exposing (view, viewEdges, viewNodes)

import Arc2d exposing (sweptAngle)
import Color exposing (Color)
import Color.Interpolate as Interpolate exposing (interpolate)
import Colors
import Direction2d exposing (Direction2d)
import EllipticalArc2d exposing (startAngle)
import Frame2d
import Geometry.Svg as Svg
import Graph exposing (Edge, Graph, Node, nodes)
import Html.Attributes exposing (selected)
import LineSegment2d exposing (LineSegment2d)
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import RewardGraph exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as Attributes
import TypedSvg exposing (..)
import TypedSvg.Attributes exposing (..)
import TypedSvg.Events exposing (onClick, onMouseOut, onMouseOver)
import TypedSvg.Types exposing (..)
import Types exposing (Msg(..))


view : Maybe Int -> Graph NodeSpec EdgeSpec -> Svg Msg
view selected graph =
    svg [ width (px 1024), height (px 768), viewBox 0 0 1024 768 ]
        [ viewEdges selected graph
        , viewNodes selected (nodes graph)
        ]



-- NODES


viewNodes : Maybe Int -> List (Node NodeSpec) -> Svg Msg
viewNodes selected nodes =
    svg [ width (px 1024), height (px 768), viewBox 0 0 1024 768 ]
        (List.map
            (\node ->
                case node.label of
                    Rectangular props ->
                        viewRectangularNode node.id selected props

                    Circular props ->
                        viewCircularNode node.id selected props
            )
            nodes
        )


viewRectangularNode : Int -> Maybe Int -> RectangularNodeSpec -> Svg Msg
viewRectangularNode current selected props =
    let
        padding =
            8

        valueDisplayHeight =
            normalizeLinear 50 150 props.value
                |> mapLinear 0 (props.height - (padding * 2))
    in
    svg
        [ x (px props.x)
        , y (px props.y)
        , width (px props.width)
        , height (px props.height)
        , onMouseOver <| NodeHovered (Just current)
        , onMouseOut <| NodeHovered Nothing
        ]
        [ rect
            [ x (px 0)
            , y (px 0)
            , rx (px 6)
            , ry (px 6)
            , width (percent 100)
            , height (percent 100)
            , fill <| Fill (nodeFill current selected props.color Colors.nodeBackground)
            ]
            []
        , rect
            [ x (px padding)
            , y (px (props.height - padding - valueDisplayHeight))
            , width (px 4)
            , height (px valueDisplayHeight)
            , fill <| Fill (interpolate Interpolate.LAB props.color Colors.nodeBackground 0.5)
            ]
            []
        , viewTextLines props.label props.color 24 0 16
        ]


mapLinear mapmin mapmax normalizedValue =
    mapmin + (normalizedValue * (mapmax - mapmin))


normalizeLinear mapmin mapmax mappedValue =
    (mappedValue - mapmin) / (mapmax - mapmin)


viewCircularNode : Int -> Maybe Int -> CircularNodeSpec -> Svg Msg
viewCircularNode current selected props =
    let
        padding =
            8

        valueDisplayHeight =
            normalizeLinear 50 150 props.value
                |> mapLinear 0 90

        valueArc =
            Arc2d.with
                { centerPoint = Point2d.fromCoordinates ( props.radius, props.radius )
                , radius = props.radius - padding
                , startAngle = degrees 160
                , sweptAngle = degrees valueDisplayHeight
                }
    in
    svg
        [ x (px <| props.cx - props.radius)
        , y (px <| props.cy - props.radius)
        , width (px <| props.radius * 2.0)
        , height (px <| props.radius * 2.0)
        , onMouseOver <| NodeHovered (Just current)
        , onMouseOut <| NodeHovered Nothing
        ]
        [ circle
            [ cx (percent 50)
            , cy (percent 50)
            , r (px props.radius)
            , fill <| Fill (nodeFill current selected props.color Colors.nodeBackground)
            ]
            []
        , image
            [ x (percent 50)
            , y (percent 50)
            , width (px props.radius)
            , height (px props.radius)
            , xlinkHref props.icon
            , transform [ Translate (-props.radius / 2) (-props.radius / 2 - 10) ]
            ]
            []
        , viewTextLines props.label props.color 24 30 16
        , Svg.arc2d
            [ stroke (interpolate Interpolate.LAB props.color Colors.nodeBackground 0.5)
            , strokeWidth (px 4)
            , fill <| FillNone
            ]
            valueArc
        ]


nodeFill : Int -> Maybe Int -> Color -> Color -> Color
nodeFill current selected colorA colorB =
    if Maybe.withDefault -1 selected == current then
        interpolate Interpolate.LAB colorA colorB 0.7

    else
        colorB


viewTextLines : List String -> Color -> Float -> Float -> Float -> Svg msg
viewTextLines lines color lineHeight baseOffset textSize =
    let
        numLines =
            toFloat (List.length lines)

        totalHeight =
            numLines * lineHeight

        offset =
            baseOffset + (lineHeight * (0.5 + numLines / 2.0) - totalHeight)
    in
    svg
        []
        (List.indexedMap
            (\index line ->
                text_
                    [ x (percent 50)
                    , y (percent 50)
                    , dy (px (offset + toFloat index * lineHeight))
                    , textAnchor AnchorMiddle
                    , alignmentBaseline AlignmentCentral
                    , fill <| Fill color
                    , fontSize (px textSize)
                    ]
                    [ Svg.text line ]
            )
            lines
        )



-- EDGES


viewEdges : Maybe Int -> Graph NodeSpec EdgeSpec -> Svg Msg
viewEdges selected graph =
    svg [ width (px 1024), height (px 768), viewBox 0 0 1024 768 ]
        (List.map
            (\edge ->
                let
                    from =
                        Graph.get edge.from graph

                    to =
                        Graph.get edge.to graph
                in
                case ( from, to ) of
                    ( Just fromContext, Just toContext ) ->
                        viewEdge selected edge fromContext.node toContext.node

                    ( _, _ ) ->
                        []
            )
            (Graph.edges graph)
            |> List.foldr (++) []
        )


viewEdge : Maybe Int -> Edge EdgeSpec -> Node NodeSpec -> Node NodeSpec -> List (Svg Msg)
viewEdge selected edge fromNode toNode =
    let
        baseColor =
            RewardGraph.color fromNode.label

        isSelected =
            Maybe.withDefault -1 selected == fromNode.id

        transferColor =
            if isSelected then
                baseColor

            else
                interpolate Interpolate.LAB baseColor Colors.nodeBackground 0.4

        trackColor =
            if isSelected then
                interpolate Interpolate.LAB baseColor Colors.nodeBackground 0.3

            else
                interpolate Interpolate.LAB baseColor Colors.nodeBackground 0.7

        fromWaypoints =
            List.map
                (Point2d.placeIn <| Frame2d.atPoint <| nodeCenter fromNode.label)
                edge.label.fromWaypoints
                |> fallbackIfEmpty (nodeCenter fromNode.label)

        toWaypoints =
            List.map
                (Point2d.placeIn <| Frame2d.atPoint <| nodeCenter toNode.label)
                edge.label.toWaypoints
                |> fallbackIfEmpty (nodeCenter toNode.label)

        polyline =
            Polyline2d.fromVertices (fromWaypoints ++ toWaypoints)

        lineLength =
            Polyline2d.length polyline

        dashLength =
            edge.label.value * 50.0

        label =
            if isSelected then
                [ viewEdgeLabel edge.label.label transferColor polyline ]

            else
                []
    in
    [ Svg.polyline2d
        [ stroke trackColor
        , strokeWidth (px 2)
        , fill <| FillNone
        , strokeDasharray "4 2"
        , strokeDashoffset (String.fromFloat <| -24 * edge.label.animationDelta)
        ]
        polyline
    , Svg.polyline2d
        [ stroke transferColor
        , strokeWidth (px 4)
        , fill <| FillNone
        , strokeDasharray
            (String.fromFloat dashLength
                ++ " "
                ++ String.fromFloat (lineLength + dashLength)
            )
        , strokeDashoffset
            (((dashLength + 10)
                + (1 - edge.label.animationDelta)
                * (lineLength + dashLength)
             )
                |> String.fromFloat
            )
        ]
        polyline
    ]
        ++ label


{-| Finds the longest segment of a polyline and places a label next to it
-}
viewEdgeLabel : EdgeLabel -> Color -> Polyline2d -> Svg msg
viewEdgeLabel label color edgeLine =
    let
        ( position, direction ) =
            pointAndDirectionAt edgeLine label.position

        rotation =
            case direction of
                Nothing ->
                    []

                Just dir ->
                    [ Rotate
                        (if Direction2d.xComponent dir == 0 then
                            -90

                         else
                            0
                        )
                        100
                        100
                    ]

        ( offX, offY ) =
            label.offset
    in
    Maybe.withDefault
        (svg [] [])
        (Maybe.map
            (\( posX, posY ) ->
                svg
                    [ x (px <| posX - 100 + offX)
                    , y (px <| posY - 100 + offY)
                    , width (px 200)
                    , height (px 200)
                    ]
                    [ g [ transform rotation ]
                        [ viewTextLines label.text color 24 0 12 ]
                    ]
            )
            (Maybe.map Point2d.coordinates position)
        )


fallbackIfEmpty : a -> List a -> List a
fallbackIfEmpty fallback list =
    case list of
        [] ->
            [ fallback ]

        _ ->
            list


pointAndDirectionAt : Polyline2d -> Float -> ( Maybe Point2d, Maybe Direction2d )
pointAndDirectionAt polyline position =
    let
        length =
            Polyline2d.length polyline

        toTravel =
            if position == 0 then
                0

            else
                length * position

        step segments distance =
            case segments of
                [] ->
                    ( Nothing, Nothing )

                seg :: rest ->
                    let
                        segLength =
                            LineSegment2d.length seg
                    in
                    if distance > segLength then
                        step rest (distance - segLength)

                    else
                        ( Just (LineSegment2d.interpolate seg (distance / segLength))
                        , LineSegment2d.direction seg
                        )
    in
    step (Polyline2d.segments polyline) toTravel
