defmodule DBConnection.Sojourn.Regulator do
  @moduledoc """
  A `:sregulator` callback module using an unlimited queue and a CoDel strategy
  for the valve.

  ### Connection options

    * `:idle_interval` - The time interval in milliseconds before the pool will
    start increasing connections, ideally the 95-99 percentile the connection
    and handshake time for a database connection (default: `100`)
    * `:idle_target` - The target idle time for a connection in milliseconds,
    ideally 5-10% of the `:idle_interval` so the connection queue does not
    shrink or grow too quickly (default: `div(idle_interval, 20)`)

  ### Underload alarm options

    * `:underload_alarm` - Either `true` to set an alarm on underload or `false`
    not to - underload means less connection processes are queuing with the
    regulator than expected and should only occur if connection processes are
    unable to connect to the database (default: `true`)
    * `:underload_target` - The target time in milliseconds for the regulator to
    wait for a connection process (default: `500`)
    * `:underload_interval` - The interval in milliseconds for an alarm to be
    set when the regulator is waiting longer than the target time for a
    connection process (default: `5_000`)
  """

  if Code.ensure_loaded?(:sregulator) do
    @behaviour :sregulator
  end

  @doc false
  def init(opts) do
    queue      = {:sbroker_drop_queue, %{}}
    conn_valve = conn_valve(opts)
    meters     = underload_meters(opts)

    {:ok, {queue, conn_valve, meters}}
  end

  ## Helpers

  defp conn_valve(opts) do
    interval = Keyword.get(opts, :idle_interval, 100)
    target   = Keyword.get(opts, :idle_target, div(interval, 20))
    size     = Keyword.get(opts, :pool_size, 10)
    overflow = Keyword.get(opts, :pool_overflow, 0)

    spec = %{target: target, interval: interval, min: size, max: size+overflow}
    {:sregulator_codel_valve, spec}
  end

  defp underload_meters(opts) do
    case Keyword.get(opts, :underload_alarm, true) do
      true ->
        target   = Keyword.get(opts, :underload_target, 500)
        interval = Keyword.get(opts, :underload_interval, 5_000)
        spec = %{alarm: alarm_id(opts), target: target, interval: interval}
        [{:sregulator_underload_meter, spec}]
      false ->
        []
    end
  end

  defp alarm_id(opts) do
    case Keyword.get(opts, :name) do
      nil  -> {:underload, Keyword.fetch!(opts, :broker_pid)}
      name -> {:underload, name}
    end
  end
end
