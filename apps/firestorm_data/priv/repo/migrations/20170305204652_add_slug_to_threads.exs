defmodule FirestormData.Repo.Migrations.AddSlugToThreads do
  use Ecto.Migration

  def change do
    alter table(:threads) do
      add :slug, :string
    end
    create unique_index(:threads, [:slug])
  end
end
