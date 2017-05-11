defmodule FirestormWeb.Web.CategoryController do
  use FirestormWeb.Web, :controller

  alias FirestormWeb.Forums

  def index(conn, _params) do
    categories = Forums.list_categories()
    render(conn, "index.html", categories: categories)
  end

  def new(conn, _params) do
    changeset = Forums.change_category(%FirestormWeb.Forums.Category{})
    render(conn, "new.html", changeset: changeset)
  end

  def create(conn, %{"category" => category_params}) do
    case Forums.create_category(category_params) do
      {:ok, category} ->
        conn
        |> put_flash(:info, "Category created successfully.")
        |> redirect(to: category_path(conn, :show, category))
      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "new.html", changeset: changeset)
    end
  end

  def show(conn, %{"id" => id}) do
    category =
      id
      |> Forums.get_category!
      |> FirestormWeb.Repo.preload(:threads)
    render(conn, "show.html", category: category)
  end

  def edit(conn, %{"id" => id}) do
    category = Forums.get_category!(id)
    changeset = Forums.change_category(category)
    render(conn, "edit.html", category: category, changeset: changeset)
  end

  def update(conn, %{"id" => id, "category" => category_params}) do
    category = Forums.get_category!(id)

    case Forums.update_category(category, category_params) do
      {:ok, category} ->
        conn
        |> put_flash(:info, "Category updated successfully.")
        |> redirect(to: category_path(conn, :show, category))
      {:error, %Ecto.Changeset{} = changeset} ->
        render(conn, "edit.html", category: category, changeset: changeset)
    end
  end

  def delete(conn, %{"id" => id}) do
    category = Forums.get_category!(id)
    {:ok, _category} = Forums.delete_category(category)

    conn
    |> put_flash(:info, "Category deleted successfully.")
    |> redirect(to: category_path(conn, :index))
  end
end
