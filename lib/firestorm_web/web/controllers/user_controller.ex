defmodule FirestormWeb.Web.UserController do
  use FirestormWeb.Web, :controller

  alias FirestormWeb.Forums

  def index(conn, params) do
    users_page = Forums.paginate_users(params["page"])
    render(conn, "index.html", users_page: users_page)
  end

  def new(conn, _params) do
    changeset = Forums.change_user(%FirestormWeb.Forums.User{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"user" => user_params}) do
    case Forums.create_user(user_params) do
      {:ok, user} ->
        conn
        |> put_flash(:info, "User created successfully.")
        |> redirect(to: user_path(conn, :show, user))
      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id} = params) do
    user = Forums.get_user!(id)
    posts_page = Forums.user_posts(user, %{page: params["page"]})
    last_post = Forums.user_last_post(user)
    last_seen = Forums.user_last_seen(user)
    render(conn, "show.html", user: user, posts_page: posts_page, last_post: last_post, last_seen: last_seen)
  end

  def edit(conn, %{"id" => id}) do
    user = Forums.get_user!(id)
    changeset = Forums.change_user(user)
    render(conn, "edit.html", user: user, changeset: changeset)
  end

  def update(conn, %{"id" => id, "user" => user_params}) do
    user = Forums.get_user!(id)

    case Forums.update_user(user, user_params) do
      {:ok, user} ->
        conn
        |> put_flash(:info, "User updated successfully.")
        |> redirect(to: user_path(conn, :show, user))
      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", user: user, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    user = Forums.get_user!(id)
    {:ok, _user} = Forums.delete_user(user)

    conn
    |> put_flash(:info, "User deleted successfully.")
    |> redirect(to: user_path(conn, :index))
  end
end
