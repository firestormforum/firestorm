defmodule FirestormWeb.CategoryIntegrationTest do
  @moduledoc false
  use FirestormWeb.IntegrationCase, async: true

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(FirestormData.Repo)
  end

  describe "With some sample data" do
    setup [:create_users, :create_categories_and_threads]

    test "Viewing a category", %{conn: conn, categories: %{elixir: elixir}} do
      conn
      |> get(category_path(conn, :show, elixir.slug))
      |> assert_response(
        status: 200,
        path: category_path(conn, :show, elixir.slug),
        html: elixir.title,
        html: hd(elixir.threads).title,
      )
    end
  end
end
