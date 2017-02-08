defmodule DataModelPlayground.Commands.LogoutTest do
  use ExUnit.Case
  alias DataModelPlayground.Commands.{Logout}

  describe "logging out" do
    # TODO: We should send some logout notification out eventually
    test "succeeds" do
      options = %Logout{ user_id: 0 }

      assert :ok = Logout.run(options)
    end
  end
end
