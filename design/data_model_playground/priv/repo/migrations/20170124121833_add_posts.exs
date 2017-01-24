defmodule DataModelPlayground.Repo.Migrations.AddPosts do
  use Ecto.Migration

  def change do
    create table(:posts) do
      add :thread_id, references(:threads)
      add :body, :text

      timestamps()
    end
    create index(:posts, [:thread_id])
  end
end
