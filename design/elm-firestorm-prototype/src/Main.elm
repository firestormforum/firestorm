module Main exposing (..)

import Html exposing (programWithFlags)
import Model exposing (Model, initialModel)
import Msg exposing (Msg(Mdl))
import Update exposing (update)
import View exposing (view)
import Material


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Material.subscriptions Mdl model
        ]


init : () -> ( Model, Cmd Msg )
init flags =
    ( initialModel flags
    , Material.init Mdl
    )
