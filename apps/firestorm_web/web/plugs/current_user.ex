defmodule FirestormWeb.Plugs.CurrentUser do
  import Plug.Conn

  def init(options), do: options

  def call(conn, _opts) do
    case get_session(conn, :current_user) do
      nil ->
        conn
      u ->
        conn
        |> assign(:current_user, u)
    end
  end
end
