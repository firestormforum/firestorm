defmodule FirestormWeb.Web.PostController do
  use FirestormWeb.Web, :controller

  require FirestormWeb.Web.Router
  alias FirestormWeb.Forums
  alias FirestormWeb.Forums.Post
  plug FirestormWeb.Web.Plugs.RequireUser when action in [:new, :create]

  def action(conn, _) do
    category = Forums.get_category!(conn.params["category_id"])
    thread = Forums.get_thread!(category, conn.params["thread_id"])
    args = [conn, conn.params, category, thread]
    apply(__MODULE__, action_name(conn), args)
  end

  def new(conn, _params, category, thread) do
    changeset =
      %Post{thread_id: thread.id}
      |> Forums.change_post
    render(conn, "new.html", changeset: changeset, category: category, thread: thread)
  end

  def create(conn, %{"post" => post_params}, category, thread) do
    case Forums.create_post(thread, current_user(conn), %{body: post_params["body"]}) do
      {:ok, _post} ->
        conn
        |> put_flash(:info, "Post created successfully.")
        |> redirect(to: category_thread_path(conn, :show, category, thread))
      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset, category: category, thread: thread)
    end
  end
end
