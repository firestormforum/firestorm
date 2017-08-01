defmodule FirestormWeb.Web.StoreChannelTest do
  use FirestormWeb.Web.ChannelCase
  alias FirestormWeb.Web.StoreChannel
  alias FirestormWeb.Web.Api.V1.FetchView
  alias FirestormWeb.Store.{ReplenishResponse, ReplenishRequest}
  alias FirestormWeb.Forums

  setup do
    {:ok, _, socket} =
      socket("user_id", %{some: :assign})
      |> subscribe_and_join(StoreChannel, "store:fetch")

    {:ok, socket: socket}
  end

  test "responds to an empty request", %{socket: socket} do
    ref = push socket, "fetch", empty_replenish_request()
    assert_response ref, %ReplenishResponse{}
  end

  test "responds with a category", %{socket: socket} do
    {:ok, elixir} = Forums.create_category(%{title: "Elixir"})

    request =
      empty_replenish_request()
      |> ReplenishRequest.request_category(elixir.id)

    ref = push socket, "fetch", request
    assert_response ref, %ReplenishResponse{categories: [elixir]}
  end

  def empty_replenish_request() do
    %ReplenishRequest{}
  end

  def assert_response(ref, response) do
    expected = FetchView.render("index.json", response)
    assert_reply ref, :ok, expected
  end
end
