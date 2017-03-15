defmodule FirestormWeb.InboundControllerTest do
  @moduledoc false

  use FirestormWeb.ConnCase
  import FirestormWeb.DataHelper

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "POST /inbound/sendgrid" do
    setup [:create_users, :create_categories_and_threads]

    test "creates post on corresponding thread", %{users: %{knewter: knewter}, categories: %{elixir: elixir}, conn: conn} do
      conn =
        conn
        |> post(inbound_path(conn, :sendgrid), %{foo: "bar"})

      assert json_response(conn, 200) =~ "ok"
    end
  end
end
