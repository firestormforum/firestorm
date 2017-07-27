defmodule FirestormWeb.Web.Plugs.Notifications do
  @moduledoc """
  A `Plug` to assign `:notifications` based on the current user
  """

  import Plug.Conn
  import FirestormWeb.Web.Session, only: [current_user: 1]
  alias FirestormWeb.Forums

  def init(options), do: options

  def call(conn, _opts) do
    case current_user(conn) do
      nil ->
        conn
        |> assign(:notifications, [])
      user ->
        conn
        |> assign(:notifications, Forums.notifications_for(user))
    end
  end
end
