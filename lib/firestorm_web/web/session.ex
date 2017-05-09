defmodule FirestormWeb.Web.Session do
  @moduledoc """
  Some helpers for session-related things
  """

  alias FirestormWeb.Forums

  def current_user(conn) do
    case Plug.Conn.get_session(conn, :current_user) do
      nil ->
        nil

      id ->
        Forums.get_user!(id)
    end
  end

  def logged_in?(conn), do: !!current_user(conn)
end
