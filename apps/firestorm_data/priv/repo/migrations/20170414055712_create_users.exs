defmodule FirestormData.Repo.Migrations.CreateUsers do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :username, :string
      add :name, :string
      add :email, :string

      timestamps()
    end
  end
end
