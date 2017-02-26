defmodule FirestormWeb.Navigation.Defaults do
  import FirestormWeb.Session
  import FirestormWeb.Router.Helpers

  defmodule NavItem do
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
