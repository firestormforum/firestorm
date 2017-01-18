module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Msg exposing (Msg(..))
import Model exposing (Model)
import Material.Layout as Layout
import Material.Icon as Icon
import Material.Color as Color
import Material.Scheme as Scheme


view : Model -> Html Msg
view model =
    div []
        [ Scheme.topWithScheme Color.Teal Color.Red <|
            Layout.render Mdl
                model.mdl
                [ Layout.fixedHeader
                , Layout.fixedDrawer
                ]
                { header = [ viewHeader model ]
                , drawer = [ viewDrawer model ]
                , tabs = ( [], [] )
                , main =
                    [ viewBody model
                    ]
                }
        , node "style" [ type_ "text/css" ] [ text styles ]
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    Layout.row
        []
        [ Layout.title [] [ text "Firestorm" ]
        , Layout.spacer
        , Layout.navigation []
            []
        ]


type alias MenuItem =
    { text : String
    , iconName : String
    }


menuItems : List MenuItem
menuItems =
    [ { text = "Dashboard", iconName = "dashboard" }
    , { text = "Users", iconName = "group" }
    , { text = "Last Activity", iconName = "alarm" }
    , { text = "Reports", iconName = "list" }
    , { text = "Organizations", iconName = "store" }
    , { text = "Project", iconName = "view_list" }
    ]


viewDrawerMenuItem : Model -> MenuItem -> Html Msg
viewDrawerMenuItem model menuItem =
    Layout.link
        []
        [ Icon.view menuItem.iconName []
        , text menuItem.text
        ]


viewDrawer : Model -> Html Msg
viewDrawer model =
    Layout.navigation [] <|
        List.map (viewDrawerMenuItem model) menuItems


viewBody : Model -> Html Msg
viewBody model =
    text "body"


styles : String
styles =
    """
    /* move the drawer to the right */
    .mdl-layout__drawer-button, .mdl-layout__drawer{
      left: initial;
      right: 0;
    }

    .mdl-layout__drawer{
      transform:translateX(250px);
    }

    .mdl-layout--fixed-drawer > .mdl-layout__content {
      margin-right: 240px;
      margin-left: 0;
    }

    .mdl-layout--fixed-drawer.is-upgraded:not(.is-small-screen) > .mdl-layout__header {
      margin-left: 0;
      margin-right: 240px;
      width: calc(100% - 240px);
    }
    """
