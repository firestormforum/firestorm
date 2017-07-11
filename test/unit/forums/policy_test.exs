defmodule FirestormWeb.Forums.PolicyTest do
  use ExUnit.Case, async: true
  alias FirestormWeb.Forums.{User, Policy}

  @alice %User{id: 1}
  @bob %User{id: 2}

  describe ":edit_user" do
    test "when current_user is some other user" do
      assert {:error, "You can only edit your own information."} = Policy.authorize(:edit_user, @alice, %{user: @bob})
    end
    test "when current_user is the user in question" do
      assert :ok = Policy.authorize(:edit_user, @alice, %{user: @alice})
    end
    test "when current_user is nil" do
      assert {:error, "You must be logged in."} = Policy.authorize(:edit_user, nil, %{user: @bob})
    end
  end
end
