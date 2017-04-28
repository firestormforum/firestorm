defmodule FirestormWeb.Web.Navigation.Defaults do
  @moduledoc """
  A module for describing the FirestormWeb navigation structure. A given
  controller can override it per-action in order to modify the menu shown in the
  drawer.
  """

  import FirestormWeb.Web.Session
  import FirestormWeb.Web.Router.Helpers

  defmodule NavItemSection do
    @moduledoc """
    A collection of NavItems in the drawer
    """

    defstruct items: []
  end

  defmodule NavItem do
    @moduledoc """
    A single nav item in the drawer.
    """

    defstruct text: "", path: "", options: [], active: false
  end

  defmacro __using__(_) do
    quote do
      def back(_template, conn), do: nil

      def nav_item_sections(_template, conn) do
        default_top_section =
          %NavItemSection{ items: [
            nav_item(conn, "Home", page_path(conn, :home)),
            nav_item(conn, "Following", followed_path(conn, :index)),
            nav_item(conn, "Participating in", "#"),
            nav_item(conn, "Recently viewed", "#"),
            nav_item(conn, "Top", "#"),
            nav_item(conn, "Mentions", "#"),
          ]}

        default_middle_section =
          %NavItemSection{ items: [
              nav_item(conn, "Categories", category_path(conn, :index)),
              nav_item(conn, "Tags", "#"),
              nav_item(conn, "Users", "#"),
          ]}

        default_bottom_section =
          if logged_in?(conn) do
              %NavItemSection{ items: [
                nav_item(conn, "Log Out", auth_path(conn, :delete), [method: :delete])
              ]}
          else
            %NavItemSection{ items: [
              nav_item(conn, "Log In", auth_path(conn, :request, :github))
            ]}
          end

        [ default_top_section, default_middle_section, default_bottom_section ]
      end

      def nav_item(conn, text, path, options \\ []) do
        current_path = conn.request_path
        %NavItem{text: text, path: path, active: current_path == path, options: options}
      end

      defoverridable [back: 2, nav_item_sections: 2]
    end
  end
end
