defmodule FirestormWeb.Web.Api.V1.FetchControllerTest do
  use FirestormWeb.ConnCase
  import FirestormWeb.DataHelper

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "with some categories" do
    setup [:create_users, :create_categories_and_threads]

    test "POST /api/v1/fetch (fetching categories)", %{conn: conn, categories: %{elixir: elixir, elm: elm}, users: %{knewter: knewter}} do
      conn = post conn, "/api/v1/fetch", %{categories: [elm.id]}
      response = json_response(conn, 200)
      assert response
      assert length(response["categories"]) == 1
      first_category = hd(response["categories"])
      assert length(first_category["thread_ids"]) == 1
    end

    test "POST /api/v1/fetch (fetching users)", %{conn: conn, categories: %{elixir: elixir, elm: elm}, users: %{knewter: knewter}} do
      conn = post conn, "/api/v1/fetch", %{users: [knewter.id]}
      response = json_response(conn, 200)
      assert response
      assert length(response["users"]) == 1
      first_user = hd(response["users"])
      assert first_user["username"] == "knewter"
    end

    test "POST /api/v1/fetch (fetching threads)", %{conn: conn, threads: %{elixir_thread: elixir_thread, elm_thread: elm_thread}, categories: %{elixir: elixir, elm: elm}} do
      conn = post conn, "/api/v1/fetch", %{threads: [elixir_thread.id, elm_thread.id]}
      response = json_response(conn, 200)
      assert response
      assert length(response["threads"]) == 2
      [thread1, thread2] = response["threads"]
      assert thread1["category_id"] == elixir.id
      assert thread2["category_id"] == elm.id
    end

    test "POST /api/v1/fetch (fetching posts)", %{conn: conn, threads: %{elixir_thread: elixir_thread, elm_thread: elm_thread}, posts: %{elixir_post: elixir_post, elm_post: elm_post}} do
      conn = post conn, "/api/v1/fetch", %{posts: [elixir_post.id, elm_post.id]}
      response = json_response(conn, 200)
      assert response
      assert length(response["posts"]) == 2
      [post1, post2] = response["posts"]
      assert post1["thread_id"] == elixir_thread.id
      assert post2["thread_id"] == elm_thread.id
    end
  end
end
