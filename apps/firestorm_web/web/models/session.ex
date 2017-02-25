defmodule FirestormWeb.Session do
  alias FirestormData.{User, Repo}

  def current_user(conn) do
    case conn.assigns[:current_user] do
      nil ->
        IO.puts "checking current user but was nil!"
        nil

      id ->
        Repo.get(User, id)
    end
  end

  def logged_in?(conn), do: !!current_user(conn)
end
