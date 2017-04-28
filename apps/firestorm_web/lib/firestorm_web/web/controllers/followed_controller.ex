defmodule FirestormWeb.Web.FollowedController do
  use FirestormWeb.Web, :controller
  alias FirestormData.Forums
  alias FirestormWeb.Web.Session
  plug FirestormWeb.Plugs.RequireUser

  def index(conn, _) do
    current_user = Session.current_user(conn)
    threads = Forums.get_followed_threads!(current_user)

    conn
    |> render("index.html", threads: threads)
  end
end
