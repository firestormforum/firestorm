defmodule FirestormWeb.Repo.Migrations.MakeUserUsernameUnique do
  use Ecto.Migration

  def change do
    create unique_index(:forums_users, [:username])
  end
end
