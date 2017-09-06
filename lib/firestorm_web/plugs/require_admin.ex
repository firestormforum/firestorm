defmodule FirestormWeb.Web.Plugs.RequireAdmin do
  @moduledoc """
  A `Plug` to redirect to `pages/index` if the current user is not an admin.
  """

  import Plug.Conn
  import Phoenix.Controller, only: [put_flash: 3, redirect: 2]
  import FirestormWeb.Web.Router.Helpers
  import FirestormWeb.Web.Session, only: [admin?: 1]

  def init(options), do: options

  def call(conn, _opts) do
    if admin?(conn) do
      conn
    else
      conn
      |> put_flash(:error, "You must be an administrator in to access this page.")
      |> redirect(to: category_path(conn, :index))
      |> halt()
    end
  end
end
