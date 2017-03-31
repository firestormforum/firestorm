defmodule FirestormData.MockEndpoint do
  def broadcast!(topic, event, payload) do
    :ok
  end
end
