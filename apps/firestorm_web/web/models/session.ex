defmodule FirestormWeb.Session do
  alias FirestormData.{User, Repo}

  def current_user(conn) do
    case Plug.Conn.get_session(conn, :current_user) do
      nil ->
        nil
      id ->
        Repo.get(User, id)
    end
  end

  def logged_in?(conn), do: !!current_user(conn)
end
