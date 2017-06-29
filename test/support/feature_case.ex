defmodule FirestormWeb.Web.FeatureCase do
  use ExUnit.CaseTemplate

  defmodule Helpers do
    use Wallaby.DSL

    def log_in_as(session, user) do
      session
      |> visit("/")
      |> Browser.set_cookie("current_user", user.id)
    end
  end

  using do
    quote do
      use Wallaby.DSL

      alias FirestormWeb.Repo
      import Ecto
      import Ecto.Changeset
      import Ecto.Query

      import FirestormWeb.Web.Router.Helpers
      import FirestormWeb.Web.FeatureCase.Helpers

      alias FirestormWeb.Forums.{
        User,
        Category,
        Thread,
        Post
      }
    end
  end

  setup tags do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(FirestormWeb.Repo)

    unless tags[:async] do
      Ecto.Adapters.SQL.Sandbox.mode(FirestormWeb.Repo, {:shared, self()})
    end

    metadata = Phoenix.Ecto.SQL.Sandbox.metadata_for(FirestormWeb.Repo, self())
    {:ok, session} = Wallaby.start_session(metadata: metadata)
    {:ok, session: session}
  end
end
