defmodule FirestormWeb.CategoryController do
  use FirestormWeb.Web, :controller
  alias FirestormData.Commands.GetCategory

  def show(conn, %{"id" => id_or_slug}) do
    case GetCategory.run(%GetCategory{finder: id_or_slug}) do
      {:ok, c} ->
        conn
        |> render("show.html", category: c)
      {:error, :not_found} ->
        conn
        |> put_flash(:error, "No such category!")
        |> redirect(to: page_path(conn, :home))
    end
  end

  def new(conn, _params) do
    changeset = Category.changeset(%Category{})

    conn
    |> render("new.html", changeset: changeset)
  end

  # FIXME: Use commands, don't just CRUD it up
  def create(conn, %{"category" => category_params}=params) do
    changeset =
      %Category{}
      |> Category.changeset(category_params)

    case changeset.valid? do
      true ->
        case Repo.insert(changeset) do
          {:ok, category} ->
            conn
            |> put_flash(:info, "Category created successfully")
            |> redirect(to: page_path(conn, :home))
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
