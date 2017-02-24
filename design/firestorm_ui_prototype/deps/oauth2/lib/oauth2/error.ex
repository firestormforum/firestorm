defmodule OAuth2.Error do
  defexception [:reason]
  def message(%__MODULE__{reason: :econnrefused}), do: "Connection refused"
  def message(%__MODULE__{reason: reason}) when is_binary(reason), do: reason
  def message(%__MODULE__{reason: reason}), do: inspect reason
end

