defmodule FirestormData.Schema.TagTest do
  alias FirestormData.{Tag, Repo}
  use ExUnit.Case

  @valid_attributes %{
    title: "OTP",
  }

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  test "it requires a title" do
    changeset =
      %Tag{}
        |> Tag.changeset(Map.delete(@valid_attributes, :title))

    refute changeset.valid?
    assert changeset.errors[:title] == {"can't be blank", [validation: :required]}
  end

  test "it generates a slug" do
    %Tag{}
    |> Tag.changeset(@valid_attributes)
    |> Repo.insert

    assert Repo.one(Tag).slug == "otp"
  end
end
