defmodule FirestormData.Repo.Migrations.CreateThreadsViews do
  use Ecto.Migration

  def change do
    create table(:threads_views) do
      add :assoc_id, references(:threads)
      add :user_id, references(:users)
      timestamps()
    end
  end
end
