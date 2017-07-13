defmodule FirestormWeb.Web.FallbackController do
  use FirestormWeb.Web, :controller

  def call(conn, {:error, reason}) do
    conn
    |> put_flash(:error, translate_policy_reason(reason))
    |> redirect(to: "/")
  end
end
