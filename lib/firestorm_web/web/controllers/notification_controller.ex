defmodule FirestormWeb.Web.NotificationController do
  use FirestormWeb.Web, :controller
  plug(FirestormWeb.Web.Plugs.RequireUser)

  alias FirestormWeb.Forums

  def index(conn, _params) do
    notifications = Forums.notifications_for(current_user(conn))
    render(conn, "index.html", notifications: notifications)
  end

  def show(conn, %{"id" => id}) do
    notification = Forums.get_notification(id)
    render(conn, "show.html", notification: notification)
  end
end
