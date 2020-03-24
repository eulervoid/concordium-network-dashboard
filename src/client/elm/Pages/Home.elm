module Pages.Home exposing (..)

import Dashboard.Formatting exposing (..)
import Dashboard.NodesTable exposing (..)
import Dashboard.Widgets exposing (..)
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Font as Font
import Element.Input as Input
import List.Extra as List
import Types exposing (..)


view : Model -> Element Msg
view model =
    content <|
        column [ spacing 30, width fill ]
            [ viewSummaryWidgets model model.nodes
            , remoteDataView model.palette
                (\nodes ->
                    let
                        listNodes =
                            nodes
                                |> Dict.toList
                                |> List.map Tuple.second

                        sortedNodes =
                            sortNodesMode model.sortMode listNodes
                    in
                    if model.window.width < 1800 then
                        content <| nodesTable model model.sortMode sortedNodes

                    else
                        let
                            ( nodes1, nodes2 ) =
                                -- Ceiling so we always end up with longer part of odd-numbered list first
                                List.splitAt (toFloat (List.length listNodes) / 2 |> ceiling) sortedNodes
                        in
                        row [ spacing 20, width fill ]
                            [ nodesTable model model.sortMode (sortNodesMode model.sortMode nodes1)
                            , column [ height fill, width (px 4), Background.color model.palette.bg1 ] []
                            , nodesTable model model.sortMode (sortNodesMode model.sortMode nodes2)
                            ]
                )
                model.nodes
            ]