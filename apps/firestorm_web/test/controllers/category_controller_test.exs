defmodule FirestormWeb.CategoryControllerTest do
  use FirestormWeb.ConnCase

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "GET /category/:id" do
    setup [:create_category]

    test "works with slug", %{category: category, conn: conn} do
      conn =
        conn
        |> get(category_path(conn, :show, category.slug))

      assert html_response(conn, 200) =~ category.title
    end

    test "works with id", %{category: category, conn: conn} do
      conn =
        conn
        |> get(category_path(conn, :show, category.id))

      assert html_response(conn, 200) =~ category.title
    end
  end

  defp create_category(_) do
    options =
      %CreateCategory{
        title: "some title"
      }

    {:ok, category_id} = CreateCategory.run(options)
    category = Repo.get(Category, category_id)
    {:ok, %{category: category}}
  end
end
