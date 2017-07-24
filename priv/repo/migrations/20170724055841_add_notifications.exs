defmodule FirestormWeb.Repo.Migrations.AddNotifications do
  use Ecto.Migration

  def change do
    create table(:forums_notifications) do
      add :body, :text
      add :user_id, references(:forums_users)
      timestamps()
    end
  end
end
