defmodule FirestormWeb.Web.Navigation.Defaults do
  @moduledoc """
  A module for describing the FirestormWeb navigation structure. A given
  controller can override it per-action in order to modify the menu shown in the
  drawer.
  """

  import FirestormWeb.Web.Session
  import FirestormWeb.Web.Router.Helpers

  defmodule NavItem do
    @moduledoc """
    A single nav item in the drawer.
    """

    defstruct text: "", path: "", options: [], active: false
  end

  defmacro __using__(_) do
    quote do
      def back(_template, conn), do: nil

      def nav_items(_template, conn) do
        defaults =
          [
            nav_item(conn, "Home", page_path(conn, :home))
          ]

        if logged_in?(conn) do
          defaults ++
            [
              nav_item(conn, "Log Out", auth_path(conn, :delete), [method: :delete])
            ]
        else
          defaults ++
            [
              nav_item(conn, "Log In", auth_path(conn, :request, :github))
            ]
        end
      end

      def nav_item(conn, text, path, options \\ []) do
        current_path = conn.request_path
        %NavItem{text: text, path: path, active: current_path == path, options: options}
      end

      defoverridable [back: 2, nav_items: 2]
    end
  end
end
