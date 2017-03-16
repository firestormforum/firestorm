defmodule FirestormWeb.DataHelper do
  @moduledoc false

  alias FirestormData.Commands.{
    CreateCategory,
    CreateThread,
    CreatePost,
    LoginOrRegisterFromGitHub,
    GetCategory,
  }

  def create_users(_) do
    {:ok, knewter} =
      %LoginOrRegisterFromGitHub{}
      |> LoginOrRegisterFromGitHub.changeset(%{username: "knewter", email: "knewter@example.org"})
      |> LoginOrRegisterFromGitHub.run()

    {:ok, %{users: %{knewter: knewter}}}
  end

  def create_categories_and_threads(%{users: %{knewter: knewter}}) do
    {:ok, elixir_id} =
      %CreateCategory{}
      |> CreateCategory.changeset(%{title: "Elixir"})
      |> CreateCategory.run()

    {:ok, elm_id} =
      %CreateCategory{}
      |> CreateCategory.changeset(%{title: "Elm"})
      |> CreateCategory.run()

    {:ok, _elixir_thread_id} =
      %CreateThread{}
      |> CreateThread.changeset(%{
        category_id: elixir_id,
        user_id: knewter.id,
        title: "First elixir thread",
        body: "This is some content for the first elixir thread post",
      })
      |> CreateThread.run

    {:ok, _elm_thread_id} =
      %CreateThread{}
      |> CreateThread.changeset(%{
        category_id: elm_id,
        user_id: knewter.id,
        title: "First elm thread",
        body: "This is some content for the first elm thread post",
      })
      |> CreateThread.run

    {:ok, elixir} =
      %GetCategory{finder: elixir_id}
      |> GetCategory.run

    {:ok, elm} =
      %GetCategory{finder: elm_id}
      |> GetCategory.run

    {:ok, %{categories: %{elixir: elixir, elm: elm}}}
  end
end
