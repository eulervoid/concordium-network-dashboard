module Pages.Chain exposing (..)

import Chain
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Explorer.View
import Network.Widgets exposing (content, viewSummaryWidgets)
import Palette
import Types exposing (Model, Msg(..))


view : Model -> Element Msg
view model =
    content <|
        column [ width fill, height fill, spacing 20 ]
            [ viewSummaryWidgets model model.networkModel.nodes
            , el
                [ width fill
                , height (fill |> minimum 200)
                , Background.color <| Palette.darkish model.palette.bg2
                , Border.color <| model.palette.bg2
                , Border.rounded 6
                , Border.width 1
                ]
                (Chain.view model model.chainModel False)
                |> Element.map ChainMsg
            , Explorer.View.view model
                model.explorerModel.blockInfo
                model.explorerModel.blockSummary
            ]
