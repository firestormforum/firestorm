defmodule FirestormWeb.ThreadController do
  use FirestormWeb.Web, :controller
  alias FirestormData.Commands.GetCategory

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
    query =
      from t in Thread,
      where: t.id == ^id
             and t.category_id == ^category.id,
      preload: [:posts]

    thread =
      query
      |> Repo.one

    conn
    |> render("show.html", thread: thread, category: category)
  end

  def new(conn, _, category) do
    changeset =
      %Thread{}
      |> Thread.changeset(%{"category_id" => category.id})

    conn
    |> render("new.html", changeset: changeset, category: category)
  end

  # FIXME: Use commands, don't just CRUD it up
  def create(conn, %{"thread" => thread_params}, category) do
    thread_params =
      thread_params
      |> Map.put("category_id", category.id)

    changeset =
      %Thread{}
      |> Thread.changeset(thread_params)

    case changeset.valid? do
      true ->
        case Repo.insert(changeset) do
          {:ok, thread} ->
            conn
            |> put_flash(:info, "Thread created successfully")
            |> redirect(to: category_thread_path(conn, :show, category.slug, thread.id))

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
