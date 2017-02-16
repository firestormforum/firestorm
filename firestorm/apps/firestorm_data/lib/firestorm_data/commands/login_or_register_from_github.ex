defmodule FirestormData.Commands.LoginOrRegisterFromGitHub do
  alias FirestormData.User

  def run(%{email: email}) do
    {:ok, %User{username: email}}
  end
end
