defmodule FirestormWeb.Web.Api.V1.AuthController do
  use FirestormWeb.Web, :controller
  alias FirestormWeb.Forums

  def identity(conn, %{"user" => user_params}) do
    case Forums.login_or_register_from_identity(%{
           username: user_params["username"],
           password: user_params["password"]
         }) do
      {:ok, user} ->
        conn
        |> json(%{"data" => %{"api_token" => user.api_token}})

      {:error, _reason} ->
        conn
        |> json(%{"error" => "Login failed"})
    end
  end
end
