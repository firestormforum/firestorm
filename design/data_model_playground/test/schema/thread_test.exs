defmodule DataModelPlayground.Schema.ThreadTest do
  alias DataModelPlayground.{Category, Thread, Repo}
  use ExUnit.Case
  import Ecto.Query

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  test "it requires a category_id" do
    changeset =
      %Thread{}
        |> Thread.changeset(%{})

    refute changeset.valid?
    assert changeset.errors[:category_id] == {"can't be blank", [validation: :required]}
  end

  test "it belongs to a category" do
    elixir = %Category{} |> Category.changeset(%{title: "Elixir"}) |> Repo.insert!
    attributes = %{
      category_id: elixir.id
    }

    changeset =
      %Thread{}
        |> Thread.changeset(attributes)

    {:ok, saved_thread} = Repo.insert(changeset)

    fetched_thread =
      Thread
      |> Repo.get(saved_thread.id)
      |> Repo.preload(:category)

    assert fetched_thread.category.id == elixir.id
  end
end
