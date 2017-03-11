defmodule FirestormWeb.AcceptanceCase do
  @moduledoc false
  use ExUnit.CaseTemplate

  using do
    quote do
      use Wallaby.DSL

      import Ecto
      import Ecto.{Changeset, Query}

      alias FirestormData.Repo
      import FirestormWeb.{Factory, AcceptanceHelpers}
      import FirestormWeb.Web.Router.Helpers
      alias FirestormWeb.Web.Endpoint
    end
  end

  setup tags do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(FirestormData.Repo)

    unless tags[:async] do
      Ecto.Adapters.SQL.Sandbox.mode(FirestormData.Repo, {:shared, self()})
    end

    metadata = Phoenix.Ecto.SQL.Sandbox.metadata_for(FirestormData.Repo, self())
    {:ok, session} = Wallaby.start_session(metadata: metadata)
    {:ok, session: session}
  end
end
