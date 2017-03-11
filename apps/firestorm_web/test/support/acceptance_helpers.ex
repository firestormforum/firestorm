defmodule FirestormWeb.AcceptanceHelpers do
  @moduledoc """
  Helpers needed in multiple acceptance tests.
  """
  use Wallaby.DSL
  import FirestormWeb.Factory
  alias FirestormData.{
    Repo,
    Category,
  }
  alias FirestormData.Commands.{
    LoginOrRegisterFromGitHub,
    CreateCategory,
  }

  def log_in_as(session, user) do
    session
    |> visit("/")
    |> Browser.set_cookie("current_user", user.id)
  end

  def create_user(_) do
    user = build(:user)

    {:ok, user} =
      LoginOrRegisterFromGitHub.run(%{username: user.username})

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
end
