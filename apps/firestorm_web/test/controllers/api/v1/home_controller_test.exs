defmodule FirestormWeb.Web.Api.V1.HomeControllerTest do
  use FirestormWeb.ConnCase
  import FirestormWeb.DataHelper

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "with no categories" do
    test "GET /", %{conn: conn} do
      conn = get conn, "/api/v1/home"
      assert json_response(conn, 200) == %{
        "categories" => []
      }
    end
  end

  describe "with some categories" do
    setup [:create_users, :create_categories_and_threads]

    test "GET /", %{conn: conn, categories: %{elixir: elixir, elm: elm}} do
      conn = get conn, "/api/v1/home"
      response = json_response(conn, 200)
      assert response
      assert length(response["categories"]) == 2
      first_category = hd(response["categories"])
      assert length(first_category["threads"]) == 1
      first_thread = hd(first_category["threads"])
      assert first_thread["user"]["username"] == "knewter"
      assert length(first_thread["posts"]) == 1
    end
  end
end
