defmodule FirestormData.Repo.Migrations.CreatePostsViews do
  use Ecto.Migration

  def change do
    create table(:posts_views) do
      add :assoc_id, references(:posts)
      add :user_id, references(:users)
      timestamps()
    end
  end
end
