defmodule FirestormWeb.DataHelper do
  @moduledoc false

  alias FirestormData.Commands.{
    CreateCategory,
    CreateThread,
    CreatePost,
    LoginOrRegisterFromGitHub,
    GetCategory,
    GetThread,
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
      %GetCategory{finder: elixir_id}
      |> GetCategory.run

    {:ok, elm} =
      %GetCategory{finder: elm_id}
      |> GetCategory.run

    {:ok, elixir_thread} =
      %GetThread{finder: elixir_thread_id, category_finder: elixir_id}
      |> GetThread.run

    {:ok, elm_thread} =
      %GetThread{finder: elm_thread_id, category_finder: elm_id}
      |> GetThread.run

    elixir_post =
      elixir_thread.posts
      |> hd

    elm_post =
      elm_thread.posts
      |> hd

    {:ok,
      %{
        categories: %{
          elixir: elixir,
          elm: elm
        },
        threads: %{
          elixir_thread: elixir_thread,
          elm_thread: elm_thread
        },
        posts: %{
          elixir_post: elixir_post,
          elm_post: elm_post
        }
      }
    }
  end
end
