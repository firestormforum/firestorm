defmodule DataModelPlayground.Repo.Migrations.AddThreads do
  use Ecto.Migration

  def change do
    create table(:threads) do
      add :category_id, references(:categories)
      add :title, :string
      add :priority, :int, default: 0

      timestamps()
    end
    create index(:threads, [:category_id])
  end
end
