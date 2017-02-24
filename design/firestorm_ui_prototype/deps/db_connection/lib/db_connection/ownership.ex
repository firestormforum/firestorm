defmodule DBConnection.OwnershipError do
  defexception [:message]

  def exception(message), do: %DBConnection.OwnershipError{message: message}
end

defmodule DBConnection.Ownership do
  @moduledoc """
  A `DBConnection.Pool` that requires explicit checkout and checking
  as a mechanism to coordinate between processes.

  ### Options

    * `:ownership_pool` - The actual pool to use to power the ownership
      mechanism. The pool is started when the ownership pool is started,
      although this option may also be given on `ownership_checkout/2`
      allowing developers to customize the pool checkout/checkin
    * `:ownership_mode` - When mode is `:manual`, all connections must
      be explicitly checked out before by using `ownership_checkout/2`.
      Otherwise, mode is `:auto` and connections are checked out
      implicitly. `{:shared, owner}` mode is also supported so
      processes are allowed on demand. On all cases, checkins are
      explicit via `ownership_checkin/2`. Defaults to `:auto`.
    * `:ownership_timeout` - The maximum time that a process is allowed to own
      a connection, default `15_000`.
    * `:ownership_log` - The `Logger.level` to log ownership changes, or `nil`
      not to log, default `nil`.

  If the `:ownership_pool` has an atom name given in the `:name` option,
  an ETS table will be created and automatically used for lookups whenever
  the name is used on checkout.

  Finally, if the `:caller` option is given on checkout with a pid and no
  pool is assigned to the current process, a connection will be allowed
  from the given pid and used on checkout with `:pool_timeout` of `:infinity`.
  This is useful when multiple tasks need to collaborate on the same
  connection (hence the `:infinity` timeout).
  """

  @behaviour DBConnection.Pool

  alias DBConnection.Ownership.Manager
  alias DBConnection.Ownership.Proxy

  ## Ownership API

  @doc """
  Explicitly checks a connection out from the ownership manager.

  It may return `:ok` if the connection is checked out.
  `{:already, :owner | :allowed}` if the caller process already
  has a connection, `:error` if it could be not checked out or
  raise if there was an error.
  """
  @spec ownership_checkout(GenServer.server, Keyword.t) ::
    :ok | {:already, :owner | :allowed} | :error | no_return
  def ownership_checkout(manager, opts) do
     case Manager.checkout(manager, opts) do
      {:init, proxy} -> Proxy.init(proxy, opts)
      {:already, _} = already -> already
    end
  end

  @doc """
  Changes the ownwership mode.

  `mode` may be `:auto`, `:manual` or `{:shared, owner}`.

  The operation will always succeed when setting the mode to
  `:auto` or `:manual`. It may fail with reason `:not_owner`
  or `:not_found` when setting `{:shared, pid}` and the
  given pid does not own any connection. May return
  `:already_shared` if another process set the ownership
  mode to `{:shared, _}` and is still alive.
  """
  @spec ownership_mode(GenServer.server, :auto | :manual | {:shared, pid}, Keyword.t) ::
    :ok | :already_shared | :not_owner | :not_found
  defdelegate ownership_mode(manager, mode, opts), to: Manager, as: :mode

  @doc """
  Checks a connection back in.

  A connection can only be checked back in by its owner.
  """
  @spec ownership_checkin(GenServer.server, Keyword.t) ::
    :ok | :not_owner | :not_found
  defdelegate ownership_checkin(manager, opts), to: Manager, as: :checkin

  @doc """
  Allows the process given by `allow` to use the connection checked out
  by `owner_or_allowed`.

  It may return `:ok` if the connection is checked out.
  `{:already, :owner | :allowed}` if the `allow` process already
  has a connection. `owner_or_allowed` may either be the owner or any
  other allowed process. Returns `:not_found` if the given process
  does not have any connection checked out.
  """
  @spec ownership_allow(GenServer.server, owner_or_allowed :: pid, allow :: pid, Keyword.t) ::
    :ok | {:already, :owner | :allowed} | :not_found
  defdelegate ownership_allow(manager, owner, allow, opts), to: Manager, as: :allow

  ## Pool callbacks

  @doc false
  def ensure_all_started(opts, type) do
    Keyword.get(opts, :ownership_pool, DBConnection.Poolboy).ensure_all_started(opts, type)
  end

  @doc false
  def start_link(module, opts) do
    Manager.start_link(module, opts)
  end

  @doc false
  def child_spec(module, opts, child_opts) do
    Supervisor.Spec.worker(Manager, [module, opts], child_opts)
  end

  @doc false
  def checkout(manager, opts) do
    case Manager.lookup(manager, opts) do
      {:init, proxy} ->
        case Proxy.init(proxy, opts) do
          :ok                 -> Proxy.checkout(proxy, opts)
          {:error, _} = error -> error
        end
      {:ok, proxy} ->
        Proxy.checkout(proxy, opts)
      :not_found ->
        case Keyword.pop(opts, :caller) do
          {nil, _} ->
            msg = """
            cannot find ownership process for #{inspect self()}.

            When using ownership, you must manage connections in one
            of the three ways:

              * By explicitly checking out a connection
              * By explicitly allowing a spawned process
              * By running the pool in shared mode

            The first two options require every new process to explicitly
            check a connection out or be allowed by calling checkout or
            allow respectively.
            
            The third option requires a {:shared, pid} mode to be set.
            If using shared mode in tests, make sure your tests are not
            async.

            If you are reading this error, it means you have not done one
            of the steps above or that the owner process has crashed.
            """
            {:error, DBConnection.OwnershipError.exception(msg)}
          {owner, opts} ->
            ownership_allow(manager, owner, self(), opts)
            checkout(manager, [pool_timeout: :infinity] ++ opts)
        end
    end
  end

  @doc false
  def checkin(proxy, state, opts) do
    Proxy.checkin(proxy, state, opts)
  end

  @doc false
  def disconnect(proxy, exception, state, opts) do
    Proxy.disconnect(proxy, exception, state, opts)
  end

  @doc false
  def stop(proxy, err, state, opts) do
    Proxy.stop(proxy, err, state, opts)
  end
end
