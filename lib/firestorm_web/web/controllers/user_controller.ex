defmodule FirestormWeb.Web.UserController do
  use FirestormWeb.Web, :controller

  alias FirestormWeb.Forums

  def index(conn, params) do
    users_page = Forums.paginate_users(params["page"])
    render(conn, "index.html", users_page: users_page)
  end

  def show(conn, %{"id" => id} = params) do
    user = Forums.get_user!(id)
    posts_page = Forums.user_posts(user, %{page: params["page"]})
    oembed_decorated_posts =
      posts_page.entries
      |> Enum.map(&Forums.decorate_post_oembeds/1)
    posts_page = %{ posts_page | entries: oembed_decorated_posts }

    last_post = Forums.user_last_post(user)
    last_seen = Forums.user_last_seen(user)
    render(conn, "show.html", user: user, posts_page: posts_page, last_post: last_post, last_seen: last_seen)
  end

  def edit(conn, %{"id" => id}) do
    {id, _} = Integer.parse(id)
    with :ok <- Bodyguard.permit(Forums, :edit_user, current_user(conn), user: %{id: id}) do
      user = Forums.get_user!(id)
      changeset = Forums.change_user(user)
      render(conn, "edit.html", user: user, changeset: changeset)
    end
  end

  def update(conn, %{"id" => id, "user" => user_params}) do
    {id, _} = Integer.parse(id)
    user = Forums.get_user!(id)
    with :ok <- Bodyguard.permit(Forums, :edit_user, current_user(conn), user: %{id: id}),
         {:ok, user} <- Forums.update_user(user, user_params) do
           conn
           |> put_flash(:info, "User updated successfully.")
           |> redirect(to: user_path(conn, :show, user))
         else
           {:error, %Ecto.Changeset{} = changeset} ->
             render(conn, "edit.html", user: user, changeset: changeset)
    end
  end
end
