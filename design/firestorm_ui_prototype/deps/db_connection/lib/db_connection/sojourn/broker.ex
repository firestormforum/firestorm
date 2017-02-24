defmodule DBConnection.Sojourn.Broker do
  @moduledoc """
  A `:sbroker` callback module using a timeout strategy for the client queue and
  a CoDel strategy for the connection queue.

  ### Queue options

    * `:queue_timeout` - The time to wait for control of the connection's
    state (default: `5_000`)
    * `:queue_out` - Either `:out` for a FIFO queue or `:out_r` for a
    LIFO queue (default: `:out`)
    * `:queue_drop` - Either `:drop` for head drop on max size or
    `:drop_r` for tail drop (default: `:drop`)
    * `:queue_size` - The maximum size of the queue (default: `1024`)
    * `:queue_min` - The minimum number of requests in the queue before the
    queue will timeout requests (default: `0`)

  ### Queue protector options

    * `:protector` - Either `true` to enable the queue protector or `false` not
    to (default: `false`)
    * `:protector_interval` - The time interval in milliseconds before the
    protector will start dropping requests, ideally the 95-99 percentile
    response time for a failure response to an end user, e.g. 95 percent
    response time for a HTTP 500 response to a mobile user (default: `5_000`)
    * `:protector_target` - The target queue time in milliseconds, ideally
    5%-10% of the `:protector_interval` for the queue to feel responsive,
    (default: `div(protector_interval, 10)`)
    * `:protector_size` - The maximum number of requests the protector will
    try to allow in the message queue (default: `64`)
    * `:protector_min` - The minimum number of requests in the message queue
    before the protector will drop requests (default: `0`)

  ### Queue overload alarm options

    * `:overload_alarm` - Either `true` to set an alarm on overload or `false`
    not to - a protector should prevent overload so defaults to `false` when
    using a protector, otherwise `true` (default: `not protector`)
    * `:overload_target` - The target time in milliseconds for messages in the
    message queue (default: `500`)
    * `:overload_interval` - The interval in milliseconds for an alarm to be set
    when messages are spending longer than the target time in the message queue
    (default: `5_000`)

  ### Idle connection options

    * `:idle_out` - Either `:out` for a FIFO queue or `:out_r` for a LIFO queue
    (default: if `pool_overflow == 0` `:out`, otherwise `:out_r`)
    * `:idle_interval` - The time interval in milliseconds before the pool will
    start pinging or dropping connections, ideally the 95-99 percentile the
    connection and handshake time for a database connection (default: `100`)
    * `:idle_min` - The minimum number of idle connections before the pool
    will ping or drop idle connections (default: `div(pool_size, 4)`)

  ### Regulator options

    * `:regulator_update` - The average interval in milliseconds to update the
    regulator (default: `100`)
  """

  if Code.ensure_loaded?(:sbroker) do
    @behaviour :sbroker
  end

  @pool {__MODULE__, :pool_sup}
  @regulator {__MODULE__, :regulator_pid}

  @doc false
  def init({broker, mod, opts}) do
    opts = Keyword.put(opts, :broker_pid, self())
    pool = ensure_pool(mod, opts)
    reg = lookup_regulator(pool)
    opts = [pool_pid: pool, regulator_pid: reg] ++ opts
    case broker do
      __MODULE__ ->
        do_init(opts)
      mod ->
        apply(mod, :init, opts)
    end
  end

  ## Helpers

  defp ensure_pool(mod, opts) do
    Process.get(@pool) || start_pool(mod, opts)
  end

  defp start_pool(mod, opts) do
    {:ok, pid} = DBConnection.Sojourn.Supervisor.start_pool(mod, opts)
    _ = Process.put(@pool, pid)
    pid
  end

  defp lookup_regulator(pool) do
    Process.get(@regulator) || do_lookup_regulator(pool)
  end

  defp do_lookup_regulator(pool) do
    pid = DBConnection.Sojourn.Pool.lookup_regulator(pool)
    _ = Process.put(@regulator, pid)
    pid
  end

  defp do_init(opts) do
    {:ok, {client_queue(opts), conn_queue(opts), meters(opts)}}
  end

  defp client_queue(opts) do
    out     = Keyword.get(opts, :queue_out, :out)
    timeout = Keyword.get(opts, :queue_timeout, 5_000)
    drop    = Keyword.get(opts, :queue_drop, :drop)
    min     = Keyword.get(opts, :queue_min, 0)
    size    = Keyword.get(opts, :queue_size, 1024)

    spec = %{out: out, timeout: timeout, drop: drop, min: min, max: size}
    {:sbroker_timeout_queue, spec}
  end

  defp conn_queue(opts) do
    pool_overflow = Keyword.get(opts, :pool_overflow, 0)
    out_default   = if pool_overflow === 0, do: :out, else: :out_r
    out           = Keyword.get(opts, :idle_out, out_default)
    timeout       = Keyword.get(opts, :idle_timeout, 1_000)
    interval      = Keyword.get(opts, :idle_interval, 100)
    min_default   = div(Keyword.get(opts, :pool_size, 10), 4)
    min           = Keyword.get(opts, :idle_size, min_default)

    spec = %{out: out, target: timeout, interval: interval, min: min}
    {:sbroker_codel_queue, spec}
  end

  defp meters(opts) do
    update_meters(opts) ++ protector_meters(opts) ++ overload_meters(opts)
  end

  defp update_meters(opts) do
    update = Keyword.get(opts, :regulator_update, 50)
    pid    = Keyword.fetch!(opts, :regulator_pid)

    spec = %{update: update}
    [{:sregulator_update_meter, [{pid, :ask_r, spec}]}]
  end

  defp protector_meters(opts) do
    case Keyword.get(opts, :protector, true) do
      true ->
        interval = Keyword.get(opts, :protector_interval, 5_000)
        target   = Keyword.get(opts, :protector_target, div(interval, 10))
        update   = Keyword.get(opts, :protector_update, 100)
        min      = Keyword.get(opts, :protector_min, 0)
        max      = Keyword.get(opts, :protector_size, 128)

        spec = %{ask: %{target: target, interval: interval},
                 update: update, min: min, max: max}
        [{:sprotector_pie_meter, spec}]
      false ->
        []
    end
  end

  defp overload_meters(opts) do
    protector? = Keyword.get(opts, :protector, true)
    case Keyword.get(opts, :overload_alarm, not protector?) do
      true ->
        target   = Keyword.get(opts, :overload_target, 500)
        interval = Keyword.get(opts, :overload_interval, 5_000)
        spec = %{alarm: alarm_id(opts), target: target, interval: interval}
        [{:sbroker_overload_meter, spec}]
      false ->
        []
    end
  end

  defp alarm_id(opts) do
    case Keyword.get(opts, :name) do
      nil  -> {:overload, self()}
      name -> {:overload, name}
    end
  end
end
