defmodule FirestormWeb.Forums.Policy do
  @behaviour Bodyguard.Policy

  def authorize(:edit_user, current_user, %{user: user}) do
    if current_user do
      if "#{current_user.id}" == "#{user.id}" do
        :ok
      else
        {:error, :unauthorized}
      end
    else
      {:error, :unauthenticated}
    end
  end
end
