module Explorer exposing (..)

import Explorer.Request exposing (..)
import Http
import RemoteData exposing (..)


type alias BlockHash =
    String


type alias Model =
    { config : Config
    , blockHash : Maybe String
    , blockInfo : RemoteData String BlockInfo
    , blockSummary : RemoteData String BlockSummary
    }


type Msg
    = ReceivedConsensusStatus (Result Http.Error ConsensusStatus)
    | ReceivedBlockInfo String (Result Http.Error BlockInfo)
    | ReceivedBlockSummary (Result Http.Error BlockSummary)


init : Config -> Model
init cfg =
    { config = cfg
    , blockHash = Nothing
    , blockInfo = NotAsked
    , blockSummary = NotAsked
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedConsensusStatus res ->
            case res of
                Ok consensusStatus ->
                    let
                        hash =
                            consensusStatus.bestBlock
                    in
                    ( model, getBlockInfo model.config hash <| ReceivedBlockInfo hash )

                Err err ->
                    ( model, Cmd.none )

        ReceivedBlockInfo hash blockInfoRes ->
            case blockInfoRes of
                Ok blockInfo ->
                    ( { model
                        | blockInfo =
                            Success blockInfo
                      }
                    , getBlockSummary model.config blockInfo.blockHash ReceivedBlockSummary
                    )

                Err _ ->
                    -- Likely, an invalid block was passed in the URL.
                    ( { model | blockInfo = Failure <| "cannot load block '" ++ hash ++ "'" }, Cmd.none )

        ReceivedBlockSummary blockSummaryResult ->
            ( { model
                | blockSummary =
                    RemoteData.fromResult blockSummaryResult
                        |> RemoteData.mapError (\_ -> "cannot load block summary")
              }
            , Cmd.none
            )
