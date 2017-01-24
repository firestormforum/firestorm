defmodule DataModelPlayground.Repo.Migrations.AddUsers do
  use Ecto.Migration

  def change do
    create table(:users) do
      add :username, :string

      timestamps()
    end
    create index(:users, [:username])
  end
end
