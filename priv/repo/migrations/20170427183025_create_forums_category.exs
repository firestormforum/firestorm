defmodule FirestormWeb.Repo.Migrations.CreateFirestormWeb.Forums.Category do
  use Ecto.Migration

  def change do
    create table(:forums_categories) do
      add :title, :string

      timestamps()
    end

  end
end
