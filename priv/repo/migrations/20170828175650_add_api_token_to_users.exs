defmodule FirestormWeb.Repo.Migrations.AddApiTokenToUsers do
  use Ecto.Migration

  def change do
    alter table(:forums_users) do
      add :api_token, :string
    end
  end
end
