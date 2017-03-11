defmodule FirestormWeb.CategoryTest do
  @moduledoc false
  use FirestormWeb.AcceptanceCase, async: true

  describe "with some categories" do
    setup [:create_user, :create_categories]

    test "authenticated users can view a category", %{session: session, user: user, elixir: elixir} do
      session
      |> log_in_as(user)
      |> visit(category_path(Endpoint, :show, elixir.slug))

      assert has?(session, Query.css(".category-header", text: elixir.title))
    end
  end
end
