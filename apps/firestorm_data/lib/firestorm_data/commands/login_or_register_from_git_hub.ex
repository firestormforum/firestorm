defmodule FirestormData.Commands.LoginOrRegisterFromGitHub do
  @moduledoc """
  A `Command` that takes data regarding a GitHub user and either logs them in or
  creates their user and logs them in.
  """

  use FirestormData.Command

  embedded_schema do
    field :username, :string
    field :email, :string
    field :name, :string
  end

  @required_fields ~w(username email)a
  @optional_fields ~w(name)a

  def changeset(record, params \\ %{}) do
    record
    |> cast(params, @required_fields ++ @optional_fields)
    |> validate_required(@required_fields)
  end

  def run(changeset) do
    case changeset.valid? do
      true ->
        %{username: username, email: email, name: name} =
          changeset
          |> apply_changes

        case find_user_by_username(username) do
          nil -> create_user(username, email, name)
          u ->
            {:ok, u} = update_details(u, email, name)
            {:ok, u}
        end

      false ->
        {:error, changeset}
    end
  end

  defp update_details(%User{} = user, email, name) do
    user
    |> User.changeset(%{email: email, name: name})
    |> Repo.update
  end

  defp find_user_by_username(username) do
    query =
      from u in User,
        where: u.username == ^username

    Repo.one query
  end

  defp create_user(username, email, name) do
    result =
      %User{}
      |> User.changeset(%{username: username, email: email, name: name})
      |> Repo.insert

    case result do
      {:ok, u} ->
        {:ok, u}
      {:error, changeset} ->
        {:error, changeset.errors}
    end
  end
end
