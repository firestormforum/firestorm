module Msg exposing (Msg(..))

import Material


type Msg
    = NoOp
    | Mdl (Material.Msg Msg)
