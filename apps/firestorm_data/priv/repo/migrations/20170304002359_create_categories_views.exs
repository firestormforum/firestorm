defmodule FirestormData.Repo.Migrations.CreateCategoriesViews do
  use Ecto.Migration

  def change do
    create table(:categories_views) do
      add :assoc_id, references(:categories)
      add :user_id, references(:users)
      timestamps()
    end
  end
end
