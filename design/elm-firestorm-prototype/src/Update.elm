module Update exposing (update)

import Msg exposing (Msg(..))
import Model exposing (Model)
import Material


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        Mdl msg ->
            Material.update Mdl msg model
