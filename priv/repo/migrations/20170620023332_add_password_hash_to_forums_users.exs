defmodule FirestormWeb.Repo.Migrations.AddPasswordHashToForumsUsers do
  use Ecto.Migration

  def change do
    alter table(:forums_users) do
      add :password_hash, :string
    end
  end
end
