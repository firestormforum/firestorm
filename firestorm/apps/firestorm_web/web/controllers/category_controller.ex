defmodule FirestormWeb.CategoryController do
  use FirestormWeb.Web, :controller

  def new(conn, _params) do
    changeset = Category.changeset(%Category{})
    render conn, "new.html", changeset: changeset
  end

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
            |> redirect to: page_path(conn, :home)
          {:error, changeset} ->
            conn
            |> render "new.html", changeset: changeset
        end
      false ->
        conn
        |> render "new.html", changeset: changeset
    end
  end
end
