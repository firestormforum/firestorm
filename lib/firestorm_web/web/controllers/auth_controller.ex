defmodule FirestormWeb.Web.AuthController do
  use FirestormWeb.Web, :controller
  alias FirestormWeb.Forums

  def delete(conn, _params) do
    conn
    |> put_flash(:info, "You have been logged out!")
    |> configure_session(drop: true)
    |> redirect(to: "/")
  end

  def callback(%{assigns: %{ueberauth_failure: fails}} = conn, _params) do
    IO.inspect fails
    conn
    |> put_flash(:error, "Failed to authenticate.")
    |> redirect(to: "/")
  end

  def callback(%{assigns: %{ueberauth_auth: auth}} = conn, _params) do
    case auth.provider do
      :github ->
        with %{name: name, nickname: nickname, email: email} <- auth.info do
          case Forums.login_or_register_from_github(%{name: name, nickname: nickname, email: email}) do
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
end
