defmodule FirestormWeb.Web.StoreChannelTest do
  use FirestormWeb.Web.ChannelCase
  alias FirestormWeb.Web.{StoreChannel, FetchView}
  alias FirestormWeb.Store.{ReplenishResponse, ReplenishRequest}

  setup do
    {:ok, _, socket} =
      socket("user_id", %{some: :assign})
      |> subscribe_and_join(StoreChannel, "store:fetch")

    {:ok, socket: socket}
  end

  test "responds with the results of a replenish request lookup", %{socket: socket} do
    ref = push socket, "fetch", empty_replenish_request()
    assert_response ref, %ReplenishResponse{}
  end

  def empty_replenish_request() do
    %ReplenishRequest{}
  end

  def assert_response(ref, response) do
    expected = FetchView.render("index.json", response)
    assert_reply ref, :ok, expected
  end
end
