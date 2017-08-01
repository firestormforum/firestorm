defmodule FirestormWeb.Web.StoreChannel do
  use FirestormWeb.Web, :channel
  alias FirestormWeb.Store.ReplenishResponse

  def join("store:fetch", payload, socket) do
    if authorized?(payload) do
      {:ok, socket}
    else
      {:error, %{reason: "unauthorized"}}
    end
  end

  defp authorized?(_) do
    true
  end

  def handle_in("fetch", replenish_request, socket) do
    {:reply, {:ok, %ReplenishResponse{}}, socket}
  end
end
