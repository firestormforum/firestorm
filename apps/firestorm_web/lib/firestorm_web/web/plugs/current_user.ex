defmodule FirestormWeb.Plugs.CurrentUser do
  @moduledoc """
  A `Plug` to move the current user's id from the session to the conn's assigns.
  """

  import Plug.Conn

  def init(options), do: options

  def call(conn, _opts) do
    case get_current_user(conn) do
      nil ->
        conn
      u ->
        conn
        |> assign(:current_user, u)
    end
  end

  # NOTE: This exists primarily to aid in acceptance tests. We can trivially set
  # cookies in Wallaby, and this way we don't have to do convoluted things to
    # encrypt a session instead.
  defp get_current_user(conn) do
    case get_session_from_cookies() do
      true ->
        case conn.cookies["current_user"] do
          nil -> get_session(conn, :current_user)
          u -> u
        end
      false ->
        get_session(conn, :current_user)
    end
  end

  defp get_session_from_cookies do
    Application.get_env(:firestorm_web, :get_session_from_cookies) || false
  end
end
