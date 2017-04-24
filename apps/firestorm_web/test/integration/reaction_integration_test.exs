defmodule FirestormWeb.ReactionIntegrationTest do
  @moduledoc false
  use FirestormWeb.IntegrationCase, async: true

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "With some sample data" do
    setup [:create_users, :create_categories_and_threads]

    test "Reacting to a post", %{conn: conn, categories: %{elixir: elixir}, users: %{knewter: knewter}} do
      first_thread = hd(elixir.threads)
      [first_post|_] = first_thread.posts

      conn
      |> login_as(knewter)
      |> get(category_thread_path(conn, :show, elixir.slug, first_thread.id))
      |> assert_response(
        status: 200,
        path: category_thread_path(conn, :show, elixir.slug, first_thread.id),
      )
      |> follow_form(
           %{
             create_reaction: %{
               emoji: "thumbsup",
             },
           },
          %{identifier: "#post-#{first_post.id}-reaction"}
      )
      |> follow_redirect()
      |> assert_response(
        status: 200,
        path: category_thread_path(conn, :show, elixir.slug, first_thread.id),
        html: "👍",
      )
    end
  end
end
