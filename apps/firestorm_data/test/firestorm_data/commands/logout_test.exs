defmodule FirestormData.Commands.LogoutTest do
  use FirestormData.UnitCase
  alias FirestormData.Commands.{Logout}

  describe "logging out" do
    test "succeeds" do
      options = %Logout{user_id: 0}

      assert :ok = Logout.run(options)
    end
  end
end
