defmodule FirestormData.ThreadTest do
  alias FirestormData.{Category, Thread, Repo}
  use ExUnit.Case

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
    {:ok, category} = %Category{title: "Elixir"} |> Repo.insert
    {:ok, category: category}
  end

  test "creating a thread", %{category: category} do
    otp_changeset =
      %Thread{}
      |> Thread.changeset(%{category_id: category.id, title: "OTP is neat"})

    assert {:ok, _} = Repo.insert otp_changeset
  end
end
