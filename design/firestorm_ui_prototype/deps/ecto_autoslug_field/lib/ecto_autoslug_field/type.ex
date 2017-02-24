defmodule EctoAutoslugField.Type do
  @moduledoc """
  This module represents a simple wrapper around ':string' Ecto type.
  """

  @behaviour Ecto.Type

  def type, do: :string

  def cast(string) when is_binary(string), do: {:ok, string}
  def cast(_), do: :error

  def load(string) when is_binary(string), do: {:ok, string}
  def load(_), do: :error

  def dump(string) when is_binary(string), do: {:ok, string}
  def dump(_), do: :error

end
