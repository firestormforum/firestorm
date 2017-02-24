defprotocol DBConnection.Query do
  @moduledoc """
  The `DBConnection.Query` protocol is responsible for preparing and
  encoding queries.
  """

  @doc """
  Parse a query.

  This function is called to parse a query term before it is prepared using a
  connection callback module.

  See `DBConnection.prepare/3`.
  """
  @spec parse(any, Keyword.t) :: any
  def parse(query, opts)

  @doc """
  Describe a query.

  This function is called to describe a query after it is prepared using a
  connection callback module.

  See `DBConnection.prepare/3`.
  """
  @spec describe(any, Keyword.t) :: any
  def describe(query, opts)

  @doc """
  Encode parameters using a query.

  This function is called to encode a query before it is executed using a
  connection callback module.

  See `DBConnection.execute/3`.
  """
  @spec encode(any, any, Keyword.t) :: any
  def encode(query, params, opts)

  @doc """
  Decode a result using a query.

  This function is called to decode a result after it is returned by a
  connection callback module.

  See `DBConnection.execute/3`.
  """
  @spec decode(any, any, Keyword.t) :: any
  def decode(query, result, opts)
end
