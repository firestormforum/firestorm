module Model exposing (Model, initialModel)

import Material


type alias Model =
    { things : String
    , mdl : Material.Model
    }


initialModel : () -> Model
initialModel flags =
    { things = "stuff"
    , mdl = Material.model
    }
