defmodule MIME do
  @moduledoc """
  Maps MIME types to file extensions and vice versa.

  MIME types can be extended in your application configuration
  as follows:

      config :mime, :types, %{
        "application/vnd.api+json" => ["json-api"]
      }

  After adding the configuration, MIME needs to be recompiled.
  If you are using mix, it can be done with:

      $ mix deps.clean mime --build
      $ mix deps.get
  """

  @compile :no_native
  @default_type "application/octet-stream"

  # Read all the MIME type mappings into the `mapping` variable.
  @external_resource "lib/mime.types"
  stream = File.stream!("lib/mime.types")

  mapping = Enum.flat_map(stream, fn (line) ->
    if String.starts_with?(line, ["#", "\n"]) do
      []
    else
      [type|exts] = line |> String.strip |> String.split
      [{type, exts}]
    end
  end)

  app = Application.get_env(:mime, :types, %{})

  mapping = Enum.reduce app, mapping, fn {k, v}, acc ->
    type = to_string(k)
    exts = Enum.map(List.wrap(v), &to_string/1)
    List.keystore(acc, type, 0, {type, exts})
  end

  @doc """
  Returns whether a MIME type is registered.

  ## Examples

      iex> MIME.valid?("text/plain")
      true

      iex> MIME.valid?("foo/bar")
      false

  """
  @spec valid?(String.t) :: boolean
  def valid?(type) do
    is_list entry(type)
  end

  @doc """
  Returns the extensions associated with a given MIME type.

  ## Examples

      iex> MIME.extensions("text/html")
      ["html", "htm"]

      iex> MIME.extensions("application/json")
      ["json"]

      iex> MIME.extensions("foo/bar")
      []

  """
  @spec extensions(String.t) :: [String.t]
  def extensions(type) do
    entry(type) || []
  end

  @doc """
  Returns the MIME type associated with a file extension. If no MIME type is
  known for `file_extension`, `#{inspect @default_type}` is returned.

  ## Examples

      iex> MIME.type("txt")
      "text/plain"

      iex> MIME.type("foobarbaz")
      #{inspect @default_type}

  """
  @spec type(String.t) :: String.t
  def type(file_extension)

  for {type, exts} <- mapping, ext <- exts do
    def type(unquote(ext)), do: unquote(type)
  end

  def type(_ext), do: @default_type

  @doc """
  Guesses the MIME type based on the path's extension. See `type/1`.

  ## Examples

      iex> MIME.from_path("index.html")
      "text/html"

  """
  @spec from_path(Path.t) :: String.t
  def from_path(path) do
    case Path.extname(path) do
      "." <> ext -> type(downcase(ext, ""))
      _ -> @default_type
    end
  end

  defp downcase(<<h, t::binary>>, acc) when h in ?A..?Z, do: downcase(t, <<acc::binary, h+32>>)
  defp downcase(<<h, t::binary>>, acc), do: downcase(t, <<acc::binary, h>>)
  defp downcase(<<>>, acc), do: acc

  # entry/1
  @spec entry(String.t) :: list(String.t)

  for {type, exts} <- mapping do
    defp entry(unquote(type)), do: unquote(exts)
  end

  defp entry(_type), do: nil
end
