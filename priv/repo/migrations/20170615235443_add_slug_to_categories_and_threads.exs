defmodule FirestormWeb.Repo.Migrations.AddSlugToCategoriesAndThreads do
  use Ecto.Migration

  def change do
    alter table(:forums_categories) do
      add :slug, :string
    end
    alter table(:forums_threads) do
      add :slug, :string
    end
    create unique_index(:forums_categories, [:slug])
    create unique_index(:forums_threads, [:slug])
  end
end
