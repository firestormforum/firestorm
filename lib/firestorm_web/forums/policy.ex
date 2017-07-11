defmodule FirestormWeb.Forums.Policy do
  @behaviour Bodyguard.Policy

  def authorize(:edit_user, current_user, %{user: user}) do
    if current_user do
      if "#{current_user.id}" == "#{user.id}" do
        :ok
      else
        {:error, "You can only edit your own information."}
      end
    else
      {:error, "You must be logged in."}
    end
  end
end
