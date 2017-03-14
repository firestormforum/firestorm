defmodule FirestormWeb.Acceptance.ThreadTest do
  @moduledoc false
  use FirestormWeb.AcceptanceCase, async: true

  describe "when authenticated" do
    setup [:create_user, :create_categories, :create_threads]

    test "can view a thread", %{session: session, user: user, elixir: elixir, elixir_thread: elixir_thread} do
      session
      |> log_in_as(user)
      |> visit(category_thread_path(Endpoint, :show, elixir.slug, elixir_thread.id))

      assert has?(session, Query.css(".thread-header", text: elixir_thread.title))
    end

    test "can tag a thread", %{session: session, user: user, elixir: elixir, elixir_thread: elixir_thread} do
      form = Query.css(".tag-form")
      title_field = Query.css(".title")
      button = Query.css("button")
      add_tag = Query.css(".add-tag")

      session
      |> log_in_as(user)
      |> visit(category_thread_path(Endpoint, :show, elixir.slug, elixir_thread.id))
      |> click(add_tag)
      |> find(form, fn(form) ->
        form
        |> fill_in(title_field, with: "functional-programming")
        |> click(button)
      end)

      assert has?(session, Query.css(".thread-header", text: elixir_thread.title))
      assert has?(session, Query.css(".tag-list .tag", text: "functional-programming"))
    end
  end
end
