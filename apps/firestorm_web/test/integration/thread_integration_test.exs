defmodule FirestormWeb.ThreadIntegrationTest do
  @moduledoc false
  use FirestormWeb.IntegrationCase, async: true

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "With some sample data" do
    setup [:create_users, :create_categories_and_threads]

    test "Viewing a thread", %{conn: conn, categories: %{elixir: elixir}} do
      first_thread = hd(elixir.threads)
      first_post = hd(first_thread.posts)

      conn
      |> get(category_thread_path(conn, :show, elixir.slug, first_thread.id))
      |> assert_response(
        status: 200,
        path: category_thread_path(conn, :show, elixir.slug, first_thread.id),
        html: first_thread.title,
        html: first_post.body,
      )
    end

    test "Making a new thread", %{conn: conn, users: %{knewter: knewter}, categories: %{elixir: elixir}} do
      conn
      |> login_as(knewter)
      |> get(category_thread_path(conn, :new, elixir.slug))
      |> assert_response(
        status: 200,
        path: category_thread_path(conn, :new, elixir.slug),
      )
      |> follow_form(
           %{
             create_thread: %{
               title: "My new thread",
               body: "The body",
             },
           },
          %{identifier: category_thread_path(conn, :create, elixir.slug)}
      )
      |> assert_response(
        status: 200,
        html: "My new thread",
        html: "The body",
      )
    end
  end
end
