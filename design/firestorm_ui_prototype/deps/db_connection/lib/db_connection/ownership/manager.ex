defmodule DBConnection.Ownership.Manager do
  @moduledoc false
  use GenServer
  require Logger

  alias DBConnection.Ownership.PoolSupervisor
  alias DBConnection.Ownership.ProxySupervisor
  alias DBConnection.Ownership.Proxy

  @timeout 5_000

  @callback start_link(module, opts :: Keyword.t) ::
    GenServer.on_start
  def start_link(module, opts) do
    pool_mod = Keyword.get(opts, :ownership_pool, DBConnection.Poolboy)
    {owner_opts, pool_opts} = Keyword.split(opts, [:name])
    GenServer.start_link(__MODULE__, {module, owner_opts, pool_mod, pool_opts}, owner_opts)
  end

  @spec checkout(GenServer.server, Keyword.t) ::
    {:init, pid} | {:already, :owner | :allowed}
  def checkout(manager, opts) do
    timeout = Keyword.get(opts, :pool_timeout, @timeout)
    GenServer.call(manager, {:checkout, opts}, timeout)
  end

  @spec checkin(GenServer.server, Keyword.t) ::
    :ok | :not_owner | :not_found
  def checkin(manager, opts) do
    timeout = Keyword.get(opts, :pool_timeout, @timeout)
    GenServer.call(manager, :checkin, timeout)
  end

  @spec mode(GenServer.server, :auto | :manual | {:shared, pid}, Keyword.t) ::
    :ok | :already_shared | :not_owner | :not_found
  def mode(manager, mode, opts)
      when mode in [:auto, :manual]
      when elem(mode, 0) == :shared and is_pid(elem(mode, 1)) do
    timeout = Keyword.get(opts, :pool_timeout, @timeout)
    GenServer.call(manager, {:mode, mode}, timeout)
  end

  @spec allow(GenServer.server, parent :: pid, allow :: pid, Keyword.t) ::
    :ok | {:already, :owner | :allowed} | :not_found
  def allow(manager, parent, allow, opts) do
    timeout = Keyword.get(opts, :pool_timeout, @timeout)
    GenServer.call(manager, {:allow, parent, allow}, timeout)
  end

  @spec lookup(GenServer.server, Keyword.t) ::
    {:ok, pid} | {:init, pid} | :not_found
  def lookup(manager, opts) when is_atom(manager) do
    client = self()
    case :ets.lookup(manager, client) do
      [{^client, proxy}] -> {:ok, proxy}
      [] -> server_lookup(manager, opts)
    end
  end

  def lookup(manager, opts) do
    server_lookup(manager, opts)
  end

  defp server_lookup(manager, opts) do
    timeout = Keyword.get(opts, :pool_timeout, @timeout)
    GenServer.call(manager, {:lookup, opts}, timeout)
  end

  ## Callbacks

  def init({module, owner_opts, pool_mod, pool_opts}) do
    ets =
      case Keyword.fetch(owner_opts, :name) do
        {:ok, name} when is_atom(name) ->
          :ets.new(name, [:named_table, :protected, read_concurrency: true])
        _ ->
          nil
      end

    pool_opts = Keyword.put(pool_opts, :pool, pool_mod)
    {:ok, pool, owner_sup} = PoolSupervisor.start_pool(module, pool_opts)
    mode = Keyword.get(pool_opts, :ownership_mode, :auto)
    log = Keyword.get(pool_opts, :ownership_log, nil)
    {:ok, %{pool: pool, owner_sup: owner_sup, checkouts: %{}, owners: %{},
            mode: mode, mode_ref: nil, ets: ets, log: log}}
  end

  def handle_call({:mode, {:shared, pid}}, _from, %{mode: {:shared, current}} = state) do
    cond do
      pid == current ->
        {:reply, :ok, state}
      Process.alive?(current) ->
        {:reply, :already_shared, state}
      true ->
        share_and_reply(state, pid)
    end
  end
  def handle_call({:mode, {:shared, pid}}, _from, state) do
    share_and_reply(state, pid)
  end
  def handle_call({:mode, mode}, _from, state) do
    {:reply, :ok, %{state | mode: mode, mode_ref: nil}}
  end

  def handle_call({:lookup, opts}, {caller, _},
                  %{checkouts: checkouts, mode: mode} = state) do
    case Map.get(checkouts, caller, :not_found) do
      {:owner, _, proxy} ->
        {:reply, {:ok, proxy}, state}
      {:allowed, _, proxy} ->
        {:reply, {:ok, proxy}, state}
      :not_found when mode == :manual ->
        {:reply, :not_found, state}
      :not_found when mode == :auto ->
        {proxy, state} = checkout(state, caller, opts)
        {:reply, {:init, proxy}, state}
      :not_found ->
        {:shared, shared} = mode
        {:owner, ref, proxy} = Map.fetch!(checkouts, shared)
        {:reply, {:ok, proxy}, owner_allow(state, caller, ref, proxy)}
    end
  end

  def handle_call(:checkin, {caller, _}, state) do
    case get_and_update_in(state.checkouts, &Map.pop(&1, caller, :not_found)) do
      {{:owner, ref, proxy}, state} ->
        Proxy.stop(proxy, caller)
        {:reply, :ok, owner_down(state, ref)}
      {{:allowed, _, _}, _} ->
        {:reply, :not_owner, state}
      {:not_found, _} ->
        {:reply, :not_found, state}
    end
  end

  def handle_call({:allow, caller, allow}, _from, %{checkouts: checkouts} = state) do
    if kind = already_checked_out(checkouts, allow) do
      {:reply, {:already, kind}, state}
    else
      case Map.get(checkouts, caller, :not_found) do
        {:owner, ref, proxy} ->
          {:reply, :ok, owner_allow(state, allow, ref, proxy)}
        {:allowed, ref, proxy} ->
          {:reply, :ok, owner_allow(state, allow, ref, proxy)}
        :not_found ->
          {:reply, :not_found, state}
      end
    end
  end

  def handle_call({:checkout, opts}, {caller, _}, %{checkouts: checkouts} = state) do
    if kind = already_checked_out(checkouts, caller) do
      {:reply, {:already, kind}, state}
    else
      {proxy, state} = checkout(state, caller, opts)
      {:reply, {:init, proxy}, state}
    end
  end

  def handle_info({:DOWN, ref, _, _, _}, state) do
    {:noreply, state |> owner_down(ref) |> unshare(ref)}
  end

  def handle_info(_msg, state) do
    {:noreply, state}
  end

  defp already_checked_out(checkouts, pid) do
    case Map.get(checkouts, pid, :not_found) do
      {:owner, _, _} -> :owner
      {:allowed, _, _} -> :allowed
      :not_found -> nil
    end
  end

  defp checkout(state, caller, opts) do
    %{pool: pool, owner_sup: owner_sup, checkouts: checkouts, owners: owners,
      ets: ets, log: log} = state
    {:ok, proxy} = ProxySupervisor.start_owner(owner_sup, caller, pool, opts)
    log && Logger.log(log, fn -> [inspect(caller), " owns proxy " | inspect(proxy)] end)
    ref = Process.monitor(proxy)
    checkouts = Map.put(checkouts, caller, {:owner, ref, proxy})
    owners = Map.put(owners, ref, {proxy, caller, []})
    ets && :ets.insert(ets, {caller, proxy})
    {proxy, %{state | checkouts: checkouts, owners: owners}}
  end

  defp owner_allow(%{ets: ets, log: log} = state, allow, ref, proxy) do
    log && Logger.log(log, fn -> [inspect(allow), " allowed on proxy " | inspect(proxy)] end)
    state = put_in(state.checkouts[allow], {:allowed, ref, proxy})
    state = update_in(state.owners[ref], fn {proxy, caller, allowed} ->
      {proxy, caller, [allow|List.delete(allowed, allow)]}
    end)
    ets && :ets.insert(ets, {allow, proxy})
    state
  end

  defp owner_down(%{ets: ets, log: log} = state, ref) do
    case get_and_update_in(state.owners, &Map.pop(&1, ref)) do
      {{proxy, caller, allowed}, state} ->
        Process.demonitor(ref, [:flush])
        entries = [caller|allowed]
        log && Logger.log(log, fn ->
          [Enum.map_join(entries, ", ", &inspect/1), " lose proxy " |
            inspect(proxy)]
        end)
        ets && Enum.each(entries, &:ets.delete(ets, &1))
        update_in(state.checkouts, &Map.drop(&1, entries))
      {nil, state} ->
        state
    end
  end

  defp share_and_reply(%{checkouts: checkouts} = state, pid) do
    case Map.get(checkouts, pid, :not_found) do
      {:owner, ref, _} ->
        {:reply, :ok, %{state | mode: {:shared, pid}, mode_ref: ref}}
      {:allowed, _, _} ->
        {:reply, :not_owner, state}
      :not_found ->
        {:reply, :not_found, state}
    end
  end

  defp unshare(%{mode_ref: ref} = state, ref) do
    %{state | mode: :manual, mode_ref: nil}
  end
  defp unshare(state, _ref) do
    state
  end
end
