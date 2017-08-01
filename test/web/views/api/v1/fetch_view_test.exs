defmodule FirestormWeb.Web.Api.V1.FetchViewTest do
  use FirestormWeb.Web.ConnCase, async: true
  alias FirestormWeb.Web.Api.V1.{FetchView, CategoryView, ThreadView, PostView, UserView}
  alias FirestormWeb.Store.ReplenishResponse
  alias FirestormWeb.Forums

  test "rendering an empty ReplenishResponse successfully" do
    result = FetchView.render("index.json", %ReplenishResponse{})
    assert %{categories: [], users: [], threads: [], posts: []} == result
  end

  test "rendering a ReplenishResponse with a category" do
    {:ok, elixir} = Forums.create_category(%{title: "Elixir"})
    result = FetchView.render("index.json", %ReplenishResponse{categories: [elixir]})
    elixir_json = CategoryView.render("show.json", elixir)
    assert %{categories: [elixir_json], users: [], threads: [], posts: []} == result
  end

  test "rendering a ReplenishResponse with a thread" do
    {:ok, elixir} = Forums.create_category(%{title: "Elixir"})
    {:ok, bob} = Forums.create_user(%{email: "bob@example.com", name: "Bob Vladbob", username: "bob"})
    {:ok, otp_is_cool} = Forums.create_thread(elixir, bob, %{title: "OTP is cool", body: "Don't you think?"})
    result = FetchView.render("index.json", %ReplenishResponse{threads: [otp_is_cool]})
    otp_is_cool_json = ThreadView.render("show.json", otp_is_cool)
    assert %{categories: [], users: [], threads: [otp_is_cool_json], posts: []} == result
  end

  test "rendering a ReplenishResponse with a post" do
    {:ok, elixir} = Forums.create_category(%{title: "Elixir"})
    {:ok, bob} = Forums.create_user(%{email: "bob@example.com", name: "Bob Vladbob", username: "bob"})
    {:ok, otp_is_cool} = Forums.create_thread(elixir, bob, %{title: "OTP is cool", body: "Don't you think?"})
    {:ok, yup} = Forums.create_post(otp_is_cool, bob, %{body: "Yup"})
    result = FetchView.render("index.json", %ReplenishResponse{posts: [yup]})
    yup_json = PostView.render("show.json", yup)
    assert %{categories: [], users: [], threads: [], posts: [yup_json]} == result
  end

  test "rendering a ReplenishResponse with a user" do
    {:ok, bob} = Forums.create_user(%{email: "bob@example.com", name: "Bob Vladbob", username: "bob"})
    result = FetchView.render("index.json", %ReplenishResponse{users: [bob]})
    bob_json = UserView.render("show.json", bob)
    assert %{categories: [], users: [bob_json], threads: [], posts: []} == result
  end
end
