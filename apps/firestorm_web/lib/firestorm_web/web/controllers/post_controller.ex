defmodule FirestormWeb.Web.PostController do
  use FirestormWeb.Web, :controller
  alias FirestormData.Commands.{GetCategory}

  def action(conn, _) do
    case GetCategory.run(%GetCategory{finder: conn.params["category_id"]}) do
      {:ok, category} ->
        thread =
          Thread
          |> where([id: ^conn.params["thread_id"], category_id: ^category.id])
          |> preload([:posts])
          |> Repo.one

        case thread do
          nil ->
            conn
            |> put_flash(:error, "No such thread")
            |> redirect(to: category_path(conn, :show, category.id))
          thread ->
            apply(__MODULE__, action_name(conn),
              [conn, conn.params, category, thread])
        end

      {:error, :not_found} ->
        conn
        |> put_flash(:error, "No such category")
        |> redirect(to: page_path(conn, :home))
    end
  end

  def new(conn, _params, category, thread) do
    changeset =
      %Post{}
      |> Post.changeset(%{})

    conn
    |> render("new.html", changeset: changeset, category: category, thread: thread)
  end

  def create(conn, %{"post" => post_params}, category, thread) do
    post_params =
      post_params
      |> Map.put("category_id", category.id)
      |> Map.put("thread_id", thread.id)
      |> Map.put("user_id", current_user(conn).id)

    changeset =
      %Post{}
      |> Post.changeset(post_params)

    case changeset.valid? do
      true ->
        case Repo.insert(changeset) do
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
