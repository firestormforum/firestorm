defmodule FirestormWeb.Web.Plugs.RequireUser do
  @moduledoc """
  A `Plug` to redirect to `pages/index` if there is no current user
  """

  import Plug.Conn
  import Phoenix.Controller, only: [put_flash: 3, redirect: 2]
  import FirestormWeb.Web.Router.Helpers
  import FirestormWeb.Web.Session, only: [logged_in?: 1]

  def init(options), do: options

  def call(conn, _opts) do
    if logged_in?(conn) do
      conn
    else
      conn
      |> put_flash(:error, "You must be logged in to access this page.")
      |> redirect(to: category_path(conn, :index))
      |> halt()
    end
  end
end
