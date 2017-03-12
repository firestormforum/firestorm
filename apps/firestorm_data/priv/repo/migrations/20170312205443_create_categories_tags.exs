defmodule FirestormData.Repo.Migrations.CreateCategoriesTags do
  use Ecto.Migration

  def change do
    create table(:categories_taggings) do
      add :assoc_id, references(:categories)
      add :tag_id, references(:tags)
      timestamps()
    end
    create unique_index(:categories_taggings, [:assoc_id, :tag_id], name: :categories_taggings_assoc_id_tag_id_index)
  end
end
