defmodule FirestormData.Repo.Migrations.CreateCategoriesAndThreadsAndPosts do
  use Ecto.Migration

  def change do
    create table(:categories) do
      add :title, :string

      timestamps()
    end

    create table(:threads) do
      add :category_id, references(:categories)
      add :title, :string

      timestamps()
    end
    create index(:threads, [:category_id])

    create table(:posts) do
      add :thread_id, references(:threads)
      add :body, :text
      add :user_id, references(:users)

      timestamps()
    end
    create index(:posts, [:thread_id])
    create index(:posts, [:user_id])
  end
end
