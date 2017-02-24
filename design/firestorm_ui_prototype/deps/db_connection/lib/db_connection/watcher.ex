defmodule DBConnection.Watcher do
  @moduledoc false

  use GenServer

  def start_link(owner) do
    GenServer.start_link(__MODULE__, owner)
  end

  def init(owner) do
    ref = Process.monitor(owner)
    try do
      Process.link(owner)
    catch
      :error, :noproc ->
        {:stop, {:shutdown, :noproc}}
    else
      _ ->
        {:ok, ref}
    end
  end

  def handle_info({:DOWN, ref, _, _, reason}, ref) do
    {:stop, {:shutdown, reason}, ref}
  end
  def handle_info(_, ref) do
    {:noreply, ref}
  end
end
