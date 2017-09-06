defmodule FirestormWeb.Repo.Migrations.AddRoleMemberships do
  use Ecto.Migration

  def change do
    create table(:forums_role_memberships) do
      add :user_id, references(:forums_users)
      add :role_id, references(:forums_roles)

      timestamps()
    end
  end
end
