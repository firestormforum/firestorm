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

    defstruct text: "", path: "", options: []
  end

  defmacro __using__(_) do
    quote do
      def back(_template, conn), do: nil

      def nav_items(_template, conn) do
        defaults =
          [
            %NavItem{text: "Home", path: page_path(conn, :home)}
          ]

        if logged_in?(conn) do
          defaults ++
            [
              %NavItem{text: "Log Out", path: auth_path(conn, :delete), options: [method: :delete]}
            ]
        else
          defaults
        end
      end

      defoverridable [back: 2, nav_items: 2]
    end
  end
end
