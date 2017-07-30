defmodule FirestormWeb.Repo.Migrations.AddSubjectAndUrlToNotifications do
  use Ecto.Migration

  def change do
    alter table(:forums_notifications) do
      add :subject, :string
      add :url, :string
    end
  end
end
