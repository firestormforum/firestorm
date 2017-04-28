defmodule FirestormWeb.Repo.Migrations.CreateFirestormWeb.Forums.Thread do
  use Ecto.Migration

  def change do
    create table(:forums_threads) do
      add :title, :string
      add :category_id, references(:forums_categories, on_delete: :nothing)

      timestamps()
    end

    create index(:forums_threads, [:category_id])
  end
end
