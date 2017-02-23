defmodule FirestormWeb.AuthController do
  use FirestormWeb.Web, :controller
  plug Ueberauth

  alias Ueberauth.Strategy.Helpers
  alias FirestormData.Commands.LoginOrRegisterFromGitHub

  def delete(conn, _params) do
    conn
    |> put_flash(:info, "You have been logged out!")
    |> configure_session(drop: true)
    |> redirect(to: "/")
  end

  def callback(%{assigns: %{ueberauth_failure: _fails}} = conn, _params) do
    conn
    |> put_flash(:error, "Failed to authenticate.")
    |> redirect(to: "/")
  end

  def callback(%{assigns: %{ueberauth_auth: auth}} = conn, _params) do
    case auth.provider do
      :github ->
        %{ name: name, nickname: nickname, email: email } = auth.info

        case LoginOrRegisterFromGitHub.run(%{username: nickname}) do
          {:ok, user} ->
            conn
            |> put_flash(:info, "Successfully authenticated.")
            |> put_session(:current_user, user.id)
            |> redirect(to: "/")

          {:error, reason} ->
            conn
            |> put_flash(:error, reason)
            |> redirect(to: "/")
        end
    end
  end
end
