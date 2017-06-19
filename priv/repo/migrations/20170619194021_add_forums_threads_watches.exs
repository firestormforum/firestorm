defmodule FirestormWeb.Repo.Migrations.AddForumsThreadsWatches do
  use Ecto.Migration

  def change do
    create table(:forums_threads_watches) do
      add :assoc_id, references(:forums_threads)
      add :user_id, references(:forums_users)
      timestamps()
    end
  end
end
