defmodule FirestormData.Repo.Migrations.CreateReactions do
  use Ecto.Migration

  def change do
    create table(:reactions) do
      add :user_id, references(:users)
      add :post_id, references(:posts)
      add :emoji, :string

      timestamps()
    end
  end
end
