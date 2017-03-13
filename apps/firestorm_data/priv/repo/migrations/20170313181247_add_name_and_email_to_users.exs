defmodule FirestormData.Repo.Migrations.AddNameAndEmailToUsers do
  use Ecto.Migration

  def change do
    alter table(:users) do
      add :name, :string
      add :email, :string
    end
  end
end
