defmodule FirestormWeb.Acceptance.CategoryTest do
  @moduledoc false
  use FirestormWeb.AcceptanceCase, async: true

  describe "when authenticated" do
    setup [:create_user, :create_categories]

    test "can view a category", %{session: session, user: user, elixir: elixir} do
      session
      |> log_in_as(user)
      |> visit(category_path(Endpoint, :show, elixir.slug))

      assert has?(session, Query.css(".category-header", text: elixir.title))
    end

    test "can tag a category", %{session: session, user: user, elixir: elixir} do
      form = Query.css(".tag-form")
      title_field = Query.css(".title")
      button = Query.css("button")

      session
      |> log_in_as(user)
      |> visit(category_path(Endpoint, :show, elixir.slug))
      |> find(form, fn(form) ->
        form
        |> fill_in(title_field, with: "functional-programming")
        |> click(button)
      end)

      assert has?(session, Query.css(".category-header", text: elixir.title))
      assert has?(session, Query.css(".tag-list .tag", text: "functional-programming"))
    end
  end
end
