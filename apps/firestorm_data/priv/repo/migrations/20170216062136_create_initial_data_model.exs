defmodule FirestormData.Repo.Migrations.CreateInitialDataModel do
  use Ecto.Migration

  def change do
    create table(:categories) do
      add :title, :string
      add :parent_id, references(:categories), null: true
      add :slug, :string

      timestamps()
    end
    create index(:categories, [:parent_id])
    create unique_index(:categories, [:slug])

    create table(:threads) do
      add :category_id, references(:categories)
      add :title, :string
      add :priority, :int, default: 0

      timestamps()
    end
    create index(:threads, [:category_id])

    create table(:users) do
      add :username, :string

      timestamps()
    end
    create unique_index(:users, [:username])

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
