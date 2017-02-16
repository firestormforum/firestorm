defmodule DataModelPlayground.Commands.LoginOrRegisterFromGitHub do
  alias DataModelPlayground.User

  def run(%{email: email}) do
    {:ok, %User{username: email}}
  end
end
