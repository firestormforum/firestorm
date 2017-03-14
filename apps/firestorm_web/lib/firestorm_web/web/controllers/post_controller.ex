defmodule FirestormWeb.Web.PostController do
  use FirestormWeb.Web, :controller
  alias FirestormData.Commands.{
    GetCategory,
    CreatePost
  }
  plug FirestormWeb.Plugs.RequireUser


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
            |> redirect(to: category_path(conn, :show, category_finder(category)))
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
      %CreatePost{}
      |> CreatePost.changeset(%{})

    conn
    |> render("new.html", changeset: changeset, category: category, thread: thread)
  end

  def create(conn, %{"create_post" => create_post_params}, category, thread) do
    create_post_params =
      create_post_params
      |> Map.put("category_id", category.id)
      |> Map.put("thread_id", thread.id)
      |> Map.put("user_id", current_user(conn).id)

    changeset =
      %CreatePost{}
      |> CreatePost.changeset(create_post_params)

    case changeset.valid? do
      true ->
        case CreatePost.run(changeset) do
          {:ok, _post_id} ->
            conn
            |> put_flash(:info, "Post created successfully")
            |> redirect(to: category_thread_path(conn, :show, category_finder(category), thread.id))

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
