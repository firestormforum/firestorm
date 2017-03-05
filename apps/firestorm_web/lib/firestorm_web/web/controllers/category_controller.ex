defmodule FirestormWeb.Web.CategoryController do
  use FirestormWeb.Web, :controller
  alias FirestormData.Commands.{GetCategory, CreateCategory}

  def show(conn, %{"id" => id_or_slug}) do
    finder = get_finder(id_or_slug)

    case GetCategory.run(%GetCategory{finder: finder}) do
      {:ok, c} ->
        conn
        |> render("show.html", category: c)
      {:error, :not_found} ->
        conn
        |> put_flash(:error, "No such category!")
        |> redirect(to: page_path(conn, :home))
    end
  end

  def new(conn, params) do
    changeset =
      %CreateCategory{}
      |> CreateCategory.changeset(params)

    conn
    |> render("new.html", changeset: changeset)
  end

  def create(conn, %{"create_category" => create_category_params}) do
    changeset =
      %CreateCategory{}
      |> CreateCategory.changeset(create_category_params)

    case changeset.valid? do
      true ->
        case CreateCategory.run(changeset) do
          {:ok, category_id} ->
            conn
            |> put_flash(:info, "Category created successfully")
            |> redirect(to: category_path(conn, :show, category_id))
          {:error, changeset} ->
            conn
            |> render("new.html", changeset: changeset)
        end
      false ->
        conn
        |> render("new.html", changeset: changeset)
    end
  end
end
