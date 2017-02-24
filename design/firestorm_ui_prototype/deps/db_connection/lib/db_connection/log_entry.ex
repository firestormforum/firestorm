defmodule DBConnection.LogEntry do
  @moduledoc """
  Struct containing log entry information.
  """

  defstruct [:call, :query, :params, :result, :pool_time, :connection_time,
    :decode_time]

  @typedoc """
  Log entry information.

    * `:call` - The `DBConnection` function called
    * `:query` - The query used by the function
    * `:params` - The params passed to the function (if any)
    * `:result` - The result of the call
    * `:pool_time` - The length of time awaiting a connection from the pool (if
    the connection was not already checked out)
    * `:connection_time` - The length of time using the connection
    * `:decode_time` - The length of time decoding the result (if decoded the
    result using `DBConnection.Query.decode/3`)

  All times are in the native time units of the VM, see
  `System.monotonic_time/0`. Falls back to `:os.timestamp/0`.
  """
  @type t :: %__MODULE__{call: atom,
                         query: any,
                         params: any,
                         result: {:ok, any} | {:ok, any, any} | {:error, Exception.t},
                         pool_time: non_neg_integer | nil,
                         connection_time: non_neg_integer,
                         decode_time: non_neg_integer | nil}

  @doc false
  def new(call, query, params, times, result) do
    entry = %__MODULE__{call: call, query: query, params: params,
      result: result}
    parse_times(times, entry)
  end

  ## Helpers

  defp parse_times([], entry), do: entry
  defp parse_times([first | times], entry) do
    {_, entry} = Enum.reduce(times, {first, entry}, &parse_time/2)
    entry
  end

  defmacrop diff(to, from) do
    if function_exported?(:erlang, :monotonic_time, 0) do
      quote do: unquote(to) - unquote(from)
    else
      quote do: max(:timer.now_diff(unquote(to), unquote(from)), 0)
    end
  end

  defp parse_time({:stop, stop} = time, {{:decode, decode}, entry}) do
    {time, %{entry | decode_time: diff(decode, stop)}}
  end
  defp parse_time({:start, start} = time, {{:stop, stop}, entry}) do
    {time, %{entry | connection_time: diff(stop, start)}}
  end
  defp parse_time({:checkout, checkout} = time, {{:start, start}, entry}) do
    {time, %{entry | pool_time: diff(start, checkout)}}
  end
end
