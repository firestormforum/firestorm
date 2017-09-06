defmodule FirestormWeb.Repo.Migrations.AddRoles do
  use Ecto.Migration

  def change do
    create table(:forums_roles) do
      add :name, :string

      timestamps()
    end
    create unique_index(:forums_roles, [:name])
  end
end
