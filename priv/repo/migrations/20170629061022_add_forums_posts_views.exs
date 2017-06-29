defmodule FirestormWeb.Repo.Migrations.AddForumsPostsViews do
  use Ecto.Migration

  def change do
    create table(:forums_posts_views) do
      add :assoc_id, references(:forums_posts)
      add :user_id, references(:forums_users)
      timestamps()
    end
  end
end
