defmodule FirestormWeb.Web.PostController do
  use FirestormWeb.Web, :controller
  alias FirestormData.Commands.{GetThread, CreatePost}

  def action(conn, _) do
    thread_finder = get_finder(conn.params["thread_id"])
    category_finder = get_finder(conn.params["category_id"])

    case GetThread.run(%GetThread{finder: thread_finder, category_finder: category_finder}) do
      {:ok, thread} ->
        apply(__MODULE__, action_name(conn),
          [conn, conn.params, thread.category, thread])

      {:error, :not_found} ->
        conn
        |> put_flash(:error, "No such thread")
        |> redirect(to: page_path(conn, :home))
    end
  end

  def new(conn, params, category, thread) do
    changeset =
      %CreatePost{}
      |> CreatePost.changeset(params)

    conn
    |> render("new.html", changeset: changeset, category: category, thread: thread)
  end

  def create(conn, %{"create_post" => create_post_params}, category, thread) do
    create_post_params =
      create_post_params
      |> Map.put("thread_id", thread.id)
      |> Map.put("user_id", current_user(conn).id)

    changeset =
      %CreatePost{}
      |> CreatePost.changeset(create_post_params)

    case changeset.valid? do
      true ->
        case CreatePost.run(changeset) do
          {:ok, _} ->
            conn
            |> put_flash(:info, "Post created successfully")
            |> redirect(to: category_thread_path(conn, :show, category.slug, thread.id))

          {:error, changeset} ->
            conn
            |> render("new.html", changeset: changeset, category: category, thread: thread)
        end
      false ->
        conn
        |> render("new.html", changeset: changeset, category: category, thread: thread)
    end
  end
end
