defmodule FirestormWeb.PostIntegrationTest do
  @moduledoc false
  use FirestormWeb.IntegrationCase, async: true

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "With some sample data" do
    setup [:create_users, :create_categories_and_threads]

    test "Making a new post", %{conn: conn, categories: %{elixir: elixir}, users: %{knewter: knewter}} do
      first_thread = hd(elixir.threads)

      conn
      |> login_as(knewter)
      |> get(category_thread_post_path(conn, :new, elixir.slug, first_thread.id))
      |> assert_response(
        status: 200,
        path: category_thread_post_path(conn, :new, elixir.slug, first_thread.id),
      )
      |> follow_form(
           %{
             create_post: %{
               body: "Something",
             },
           },
          %{identifier: category_thread_post_path(conn, :create, elixir.slug, first_thread.id)}
      )
      |> follow_redirect()
      |> assert_response(
        status: 200,
        html: "Something",
      )
    end
  end
end
