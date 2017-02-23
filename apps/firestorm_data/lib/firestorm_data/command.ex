defmodule FirestormData.Command do
  defmacro __using__(_opts) do
    quote do
      alias FirestormData.{Repo, User, Category, Thread, Post}
      import Ecto.Query
      alias Ecto.Multi
      use Ecto.Schema
      import Ecto.Changeset
      alias Ecto.Changeset
    end
  end
end
