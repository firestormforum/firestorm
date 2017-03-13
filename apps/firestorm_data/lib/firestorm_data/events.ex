defmodule FirestormData.Events do
  @moduledoc """
  This is a GenStage Producer for events that come into our system, so we can subscribe to those and respond later.
  """

  use GenStage

  def start_link() do
    GenStage.start_link(__MODULE__, [], name: __MODULE__)
  end

  def notify(event) do
    GenStage.cast(__MODULE__, {:notify, event})
  end

  ### Server API
  def init([]) do
    # Choosing a list right now because that way if the system is loaded up the
    # new notifications still go out fairly quickly.
    {:producer, {[], 0}, dispatcher: GenStage.BroadcastDispatcher}
  end

  def handle_cast({:notify, event}, {list, pending_demand}) do
    list = [event | list]
    dispatch_events(list, pending_demand, [])
  end

  def handle_demand(incoming_demand, {list, pending_demand}) do
    dispatch_events(list, incoming_demand + pending_demand, [])
  end

  defp dispatch_events(list, 0, events) do
    {:noreply, events, {list, 0}}
  end
  defp dispatch_events([], demand, events) do
    {:noreply, events, {[], demand}}
  end
  defp dispatch_events([event|rest], demand, events) do
    dispatch_events(rest, demand - 1, [event|events])
  end
end
