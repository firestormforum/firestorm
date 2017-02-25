defmodule FirestormWeb.ConnCase do
  @moduledoc """
  This module defines the test case to be used by
  tests that require setting up a connection.

  Such tests rely on `Phoenix.ConnTest` and also
  import other functionality to make it easier
  to build and query models.

  Finally, if the test case interacts with the database,
  it cannot be async. For this reason, every test runs
  inside a transaction which is reset at the beginning
  of the test unless the test case is marked as async.
  """

  use ExUnit.CaseTemplate

  using do
    quote do
      # Import conveniences for testing with connections
      use Phoenix.ConnTest

      import FirestormWeb.Router.Helpers
      alias FirestormData.Commands.CreateCategory
      alias FirestormData.{Category, User, Thread, Post, Repo}

      # The default endpoint for testing
      @endpoint FirestormWeb.Endpoint

      def login_as(conn, user) do
        conn
        |> bypass_through(FirestormWeb.Router, [:browser])
        |> get("/")
        |> fetch_session
        |> put_session(:current_user, user.id)
        |> send_resp(200, "Flush the session")
        |> recycle
      end
    end
  end

  setup tags do

    _ = tags

    {:ok, conn: Phoenix.ConnTest.build_conn()}
  end
end
