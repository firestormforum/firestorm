defmodule FirestormWeb.HomeTest do
  @moduledoc false
  use FirestormWeb.AcceptanceCase, async: true

  describe "with some categories" do
    setup [:create_user, :create_categories]

    test "authenticated users can see the category titles", %{session: session, user: user, elixir: elixir, elm: elm} do
      session
      |> log_in_as(user)
      |> visit("/home")

      assert has?(session, Query.css(".page-content", text: elixir.title))
      assert has?(session, Query.css(".page-content", text: elm.title))
    end
  end
end
