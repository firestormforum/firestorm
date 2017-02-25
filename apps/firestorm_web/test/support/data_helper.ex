defmodule FirestormWeb.DataHelper do
  alias FirestormData.Commands.{
    CreateCategory,
    CreateThread,
    LoginOrRegisterFromGitHub,
    GetCategory,
  }

  def create_users(_) do
    {:ok, knewter} =
      LoginOrRegisterFromGitHub.run(%{username: "knewter"})

    {:ok, %{users: %{ knewter: knewter}}}
  end

  def create_categories_and_threads(%{users: %{knewter: knewter}}) do
    {:ok, elixir_id} =
      %CreateCategory{title: "Elixir"}
      |> CreateCategory.run()

    {:ok, elm_id} =
      %CreateCategory{title: "Elm"}
      |> CreateCategory.run()

    {:ok, elixir_thread_id} =
      %CreateThread{}
      |> CreateThread.changeset(%{
        category_id: elixir_id,
        user_id: knewter.id,
        title: "First elixir thread",
        body: "This is some content for the first elixir thread post",
      })
      |> CreateThread.run

    {:ok, elm_thread_id} =
      %CreateThread{}
      |> CreateThread.changeset(%{
        category_id: elm_id,
        user_id: knewter.id,
        title: "First elm thread",
        body: "This is some content for the first elm thread post",
      })
      |> CreateThread.run

    {:ok, elixir} =
      %GetCategory{ finder: elixir_id }
      |> GetCategory.run

    {:ok, elm} =
      %GetCategory{ finder: elm_id }
      |> GetCategory.run

    {:ok, %{categories: %{elixir: elixir, elm: elm}}}
  end
end
