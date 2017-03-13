defmodule FirestormWeb.AcceptanceHelpers do
  @moduledoc """
  Helpers needed in multiple acceptance tests.
  """
  use Wallaby.DSL
  import FirestormWeb.Factory
  alias FirestormData.{
    Repo,
    Category,
    Thread,
  }
  alias FirestormData.Commands.{
    LoginOrRegisterFromGitHub,
    CreateCategory,
    CreateThread,
  }

  def log_in_as(session, user) do
    session
    |> visit("/")
    |> Browser.set_cookie("current_user", user.id)
  end

  def create_user(_) do
    user = build(:user)

    {:ok, user} =
      %LoginOrRegisterFromGitHub{}
      |> LoginOrRegisterFromGitHub.changeset(%{username: user.username, email: user.email})
      |> LoginOrRegisterFromGitHub.run()

    {:ok, user: user}
  end

  def create_categories(_) do
    with elixir <- create_category("Elixir"),
         elm <- create_category("Elm"),
     do: {:ok, %{elixir: elixir, elm: elm}}
  end

  def create_category(title) do
    {:ok, c_id} =
      %CreateCategory{}
      |> CreateCategory.changeset(%{title: title})
      |> CreateCategory.run()

    Repo.get(Category, c_id)
  end

  def create_threads(%{elixir: elixir, elm: elm, user: user}) do
    with elixir_thread <- create_thread(elixir, user, "First elixir thread", "This is some content for the first elixir thread post"),
         elm_thread <- create_thread(elm, user, "First elm thread", "This is some content for the first elm thread post"),
      do: {:ok, %{elixir_thread: elixir_thread, elm_thread: elm_thread}}
  end

  def create_thread(category, user, title, body) do
    {:ok, t_id} =
      %CreateThread{}
      |> CreateThread.changeset(%{
        category_id: category.id,
        user_id: user.id,
        title: title,
        body: body
      })
      |> CreateThread.run

    Repo.get(Thread, t_id)
  end
end
