defmodule FirestormData.CategoryTest do
  alias FirestormData.{Category, Repo}
  use ExUnit.Case

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  test "creating a category" do
    elixir_changeset =
      %Category{}
      |> Category.changeset(%{title: "Elixir"})

    assert {:ok, _} = Repo.insert elixir_changeset
  end
end
