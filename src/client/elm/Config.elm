module Config exposing (Config, defaultConfig, parseEnv)


type alias Config =
    { collectorUrl : String
    , middlewareUrl : String
    }


type Environment
    = Development DevelopmentTarget
    | Production


type DevelopmentTarget
    = Local
    | Staging
    | Testnet


devTarget : DevelopmentTarget
devTarget =
    -- Tweak me when developing locally to test
    Staging


parseEnv : Bool -> Environment
parseEnv isProduction =
    if isProduction then
        Production

    else
        Development devTarget


defaultConfig : Environment -> Config
defaultConfig env =
    { collectorUrl = defaultCollectorUrl env
    , middlewareUrl = defaultMiddlewareUrl env
    }


defaultCollectorUrl : Environment -> String
defaultCollectorUrl env =
    case env of
        Development target ->
            developmentUrl target "http://127.0.0.1:12000"

        -- Once deployed the routing for both collector and middleware is through the same URL
        Production ->
            ""


defaultMiddlewareUrl : Environment -> String
defaultMiddlewareUrl env =
    case env of
        Development target ->
            developmentUrl target "http://localhost:8081"

        -- Once deployed the routing for both collector and middleware is through the same URL
        Production ->
            ""


developmentUrl : DevelopmentTarget -> String -> String
developmentUrl mode localUrl =
    case mode of
        Local ->
            localUrl

        Staging ->
            "https://dashboard.eu.staging.concordium.com"

        Testnet ->
            "https://dashboard.testnet.concordium.com"
