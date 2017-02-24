defmodule OAuth2.Serializer do
  @moduledoc false

  require Logger

  defmodule NullSerializer do
    @moduledoc false

    @doc false
    def decode!(content), do: content

    @doc false
    def encode!(content), do: content
  end

  def decode!(content, type), do: serializer(type).decode!(content)

  def encode!(content, type), do: serializer(type).encode!(content)

  defp serializer(type) do
    serializer = Map.get(configured_serializers(), type, NullSerializer)
    warn_missing_serializer = Application.get_env(:oauth2, :warn_missing_serializer, true)

    if serializer == NullSerializer && warn_missing_serializer do
      Logger.warn """

      A serializer was not configured for content-type '#{type}'.

      To remove this warning for this content-type, add the following to your `config.exs` file:

          config :oauth2,
            serializers: %{
              "#{type}" => MySerializer
            }

      To remove this warning entirely, add the following to you `config.exs` file:

          config :oauth2,
            warn_missing_serializer: false
      """
    end

    serializer
  end

  defp configured_serializers do
    Application.get_env(:oauth2, :serializers) ||
      raise("Missing serializers configuration! Make sure oauth2 app is added to mix application list")
  end
end
