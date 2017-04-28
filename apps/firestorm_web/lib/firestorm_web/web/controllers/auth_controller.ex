defmodule FirestormWeb.Web.AuthController do
  use FirestormWeb.Web, :controller
  plug Ueberauth

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
        with %{name: name, nickname: nickname, email: email} <- auth.info do

          changeset =
            %LoginOrRegisterFromGitHub{}
            |> LoginOrRegisterFromGitHub.changeset(%{username: nickname, name: name, email: email})

          case LoginOrRegisterFromGitHub.run(changeset) do
            {:ok, user} ->
              conn
              |> put_flash(:info, "Successfully authenticated.")
              |> put_session(:current_user, user.id)
              |> redirect(to: "/")

            {:error, changeset} ->
              conn
              |> put_flash(:error, inspect changeset)
              |> redirect(to: "/")
          end
        else
          %{ email: email } -> show_github_oauth_required_field_error(conn, "username")
    end
  end

  defp show_github_oauth_required_field_error(conn, field) do
    conn
    |> put_flash(:error, "#{field} is a required field. Please make sure your Github profile has made #{field} publicly viewable before attempting to log in again")
    |> redirect(to: "/")
  end
end
