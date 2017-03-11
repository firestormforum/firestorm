defmodule FirestormData.Repo.Migrations.CreateThreadsTags do
  use Ecto.Migration

  def change do
    create table(:tags) do
      add :title, :string
      add :slug, :string

      timestamps()
    end
    create unique_index(:tags, [:slug])

    create table(:threads_taggings) do
      add :assoc_id, references(:threads)
      add :tag_id, references(:tags)
      timestamps()
    end
  end
end
