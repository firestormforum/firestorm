defmodule FirestormWeb.Session do
  @moduledoc """
  Some helpers for session-related things
  """

  alias FirestormData.{User, Repo}

  def current_user(conn) do
    case conn.assigns[:current_user] do
      nil ->
        nil

      id ->
        Repo.get(User, id)
    end
  end

  def logged_in?(conn), do: !!current_user(conn)
end
