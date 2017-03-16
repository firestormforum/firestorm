defmodule FirestormWeb.Web.Api.V1.CategoryControllerTest do
  use FirestormWeb.ConnCase
  import FirestormWeb.DataHelper

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "with some categories" do
    setup [:create_users, :create_categories_and_threads]

    test "GET /api/v1/categories/:id", %{conn: conn, categories: %{elixir: elixir, elm: elm}, users: %{knewter: knewter}} do
      conn = get conn, "/api/v1/categories/#{elm.id}"
      response = json_response(conn, 200)
      assert response
      assert length(response["categories"]) == 1
      first_category = hd(response["categories"])
      assert length(first_category["thread_ids"]) == 1
      threads = response["threads"]
      first_category_first_thread_id = hd(first_category["thread_ids"])
      first_category_first_thread =
        threads
        |> Enum.filter(fn(t) -> t["id"] == first_category_first_thread_id end)
        |> hd
      assert first_category_first_thread["user_id"] == knewter.id
      assert length(first_category_first_thread["post_ids"]) == 1
      assert first_category_first_thread["title"] == "First elm thread"
      first_category_first_thread_first_post_id = hd(first_category_first_thread["post_ids"])
      posts = response["posts"]
      first_category_first_thread_first_post =
        posts
        |> Enum.filter(fn(p) -> p["id"] == first_category_first_thread_first_post_id end)
        |> hd
      assert first_category_first_thread_first_post["body"] == "This is some content for the first elm thread post"
      first_category_first_thread_first_post_user_id = first_category_first_thread_first_post["user_id"]
      users = response["users"]
      first_category_first_thread_first_post_user =
        users
        |> Enum.filter(fn(p) -> p["id"] == first_category_first_thread_first_post_user_id end)
        |> hd
      assert first_category_first_thread_first_post_user["username"] == "knewter"
    end
  end
end
