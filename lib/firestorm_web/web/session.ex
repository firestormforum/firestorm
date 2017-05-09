defmodule FirestormWeb.Web.Session do
  @moduledoc """
  Some helpers for session-related things
  """

  alias FirestormData.Forums

  def current_user(conn) do
    case conn.assigns[:current_user] do
      nil ->
        nil

      id ->
        Forums.get_user!(id)
    end
  end

  def logged_in?(conn), do: !!current_user(conn)
end
