defmodule FirestormWeb.ThreadController do
  use FirestormWeb.Web, :controller
  alias FirestormData.Commands.{GetCategory, CreateThread}

  def action(conn, _) do
    case GetCategory.run(%GetCategory{finder: conn.params["category_id"]}) do
      {:ok, category} ->
        apply(__MODULE__, action_name(conn),
          [conn, conn.params, category])

      {:error, :not_found} ->
        conn
        |> put_flash(:error, "No such category")
        |> redirect(to: page_path(conn, :home))
    end
  end

  def show(conn, %{"id" => id}, category) do
    thread =
      Thread
      |> where([id: ^id, category_id: ^category.id])
      |> preload([posts: [:user]])
      |> Repo.one

    [first_post | posts] = thread.posts

    conn
    |> render(
         "show.html",
         thread: thread,
         category: category,
         first_post:
         first_post, posts: posts
       )
  end

  def new(conn, _, category) do
    changeset =
      %CreateThread{}
      |> CreateThread.changeset(%{})

    conn
    |> render("new.html", changeset: changeset, category: category)
  end

  def create(conn, %{"create_thread" => create_thread_params}, category) do
    create_thread_params =
      create_thread_params
      |> Map.put("category_id", category.id)
      |> Map.put("user_id", current_user(conn).id)

    changeset =
      %CreateThread{}
      |> CreateThread.changeset(create_thread_params)

    case changeset.valid? do
      true ->
        case CreateThread.run(changeset) do
          {:ok, thread_id} ->
            conn
            |> put_flash(:info, "Thread created successfully")
            |> redirect(to: category_thread_path(conn, :show, category.slug, thread_id))

          {:error, changeset} ->
            conn
            |> render("new.html", changeset: changeset, category: category)
        end

      false ->
        conn
        |> render("new.html", changeset: changeset, category: category)
    end
  end
end
