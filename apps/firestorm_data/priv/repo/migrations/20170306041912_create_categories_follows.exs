defmodule FirestormData.Repo.Migrations.CreateCategoriesFollows do
  use Ecto.Migration

  def change do
    create table(:categories_follows) do
      add :assoc_id, references(:categories)
      add :user_id, references(:users)
      timestamps()
    end
  end
end
