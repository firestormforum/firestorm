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

    test "replying to a thread redirects if not logged in", %{session: session, elixir: elixir, elixir_thread: elixir_thread} do
      session
      |> visit(category_thread_post_path(Endpoint, :new, elixir.slug, elixir_thread.id))
      |> take_screenshot

      assert has?(session, Query.css(".alert-box.-info", text: "Please log in to perform this action."))
    end

    test "can tag a thread", %{session: session, user: user, elixir: elixir, elixir_thread: elixir_thread} do
      form = Query.css(".tag-form")
      title_field = Query.css(".title")
      button = Query.css("button")
      add_tag = Query.css(".add-tag")

      session =
        session
        |> log_in_as(user)
        |> visit(category_thread_path(Endpoint, :show, elixir.slug, elixir_thread.id))

      # wait for js to run, sorry!
      :timer.sleep(1_000)

      session
      |> click(add_tag)
      |> find(form, fn(form) ->
        form
        |> fill_in(title_field, with: "functional-programming")
        |> click(button)
      end)

      # wait for js to run, sorry!
      :timer.sleep(1_000)

      assert has?(session, Query.css(".thread-header", text: elixir_thread.title))
      assert has?(session, Query.css(".tag-list .tag", text: "functional-programming"))
    end

    test "when viewing a thread, I'm shown the first unread post first", %{session: session, user: user, elixir: elixir, elixir_thread: elixir_thread} do
      session =
        session
        |> log_in_as(user)
        |> visit(category_thread_path(Endpoint, :show, elixir.slug, elixir_thread.id))

      post = create_post(elixir_thread)

      session =
        session
        |> visit(category_thread_path(Endpoint, :show, elixir.slug, elixir_thread.id))

      assert has?(session, Query.css("#post-#{post.id}"))
      assert has?(session, Query.css("#post-#{post.id}.newest"))
    end
  end
end
