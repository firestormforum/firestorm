defmodule FirestormWeb.Forums.Policy do
  @behaviour Bodyguard.Policy

  def authorize(:edit_user, %{id: user_id}, %{user: %{id: user_id}}) do
    :ok
  end

  # Catch-all: deny everything else
  def authorize(_, nil, _), do: {:error, :unauthenticated}
  def authorize(_, _, _), do: {:error, :unauthorized}
end
