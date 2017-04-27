defmodule FirestormWeb.Repo.Migrations.CreateFirestormWeb.Forums.User do
  use Ecto.Migration

  def change do
    create table(:forums_users) do
      add :username, :string
      add :email, :string
      add :name, :string

      timestamps()
    end

  end
end
