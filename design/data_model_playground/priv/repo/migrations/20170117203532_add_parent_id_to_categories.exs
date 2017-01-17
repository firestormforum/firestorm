defmodule DataModelPlayground.Repo.Migrations.AddParentIdToCategories do
  use Ecto.Migration

  def change do
    alter table(:categories) do
      add :parent_id, references(:categories), null: true
    end
    create index(:categories, [:parent_id])
  end
end
