defmodule FirestormWeb.PageIntegrationTest do
  @moduledoc false
  use FirestormWeb.IntegrationCase, async: true

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  test "Login page", %{conn: conn} do
    # get the root index page
    conn
    |> get(page_path(conn, :index))
    |> assert_response(
      status: 200,
      path: page_path(conn, :index),
      html: "Log in with GitHub",
      html: "Create Account",
      html: "Sign In",
      html: "By signing up"
    )
  end

  describe "Viewing the home page with some categories and threads" do
    setup [:create_users, :create_categories_and_threads]

    test "Home page", %{conn: conn, categories: %{elixir: elixir, elm: elm}} do
      # get the home page
      conn
      |> get(page_path(conn, :home))
      |> assert_response(
        status: 200,
        path: page_path(conn, :home),
        html: elixir.title,
        html: hd(elixir.threads).title,
        html: elm.title,
        html: hd(elm.threads).title,
      )
    end
  end
end
