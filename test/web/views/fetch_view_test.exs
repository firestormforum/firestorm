defmodule FirestormWeb.Web.FetchViewTest do
  use FirestormWeb.Web.ConnCase, async: true
  alias FirestormWeb.Web.FetchView
  alias FirestormWeb.Store.ReplenishResponse

  test "rendering a ReplenishResponse successfully" do
    result = FetchView.render("index.json", %ReplenishResponse{})
    assert %{categories: [], users: [], threads: [], posts: []} == result
  end
end
