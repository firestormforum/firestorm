defmodule FirestormWeb.Repo.Migrations.CreateFirestormWeb.Forums.Post do
  use Ecto.Migration

  def change do
    create table(:forums_posts) do
      add :body, :text
      add :thread_id, references(:forums_threads, on_delete: :delete_all)
      add :user_id, references(:forums_users, on_delete: :nothing)

      timestamps()
    end

    create index(:forums_posts, [:thread_id])
    create index(:forums_posts, [:user_id])
  end
end
