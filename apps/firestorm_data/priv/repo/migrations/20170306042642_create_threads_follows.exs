defmodule FirestormData.Repo.Migrations.CreateThreadsFollows do
  use Ecto.Migration

  def change do
    create table(:threads_follows) do
      add :assoc_id, references(:threads)
      add :user_id, references(:users)
      timestamps()
    end
  end
end
