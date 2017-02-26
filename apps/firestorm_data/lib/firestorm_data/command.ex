defmodule FirestormData.Command do
  defmacro __using__(_opts) do
    quote do
      alias FirestormData.{Repo, User, Category, Thread, Post}
      import Ecto.{Query, Changeset}
      alias Ecto.{Multi, Changeset}
      use Ecto.Schema
    end
  end
end
