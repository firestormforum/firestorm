defmodule FirestormData.Command do
  @moduledoc """
  A unit for modifying the Firestorm data model in a defined manner.
  """

  defmacro __using__(_opts) do
    quote do
      alias FirestormData.{
        Repo,
        User,
        Category,
        Thread,
        Post,
        Viewable,
        Followable,
        Taggable
      }
      import Ecto.{Query, Changeset}
      alias Ecto.{Multi, Changeset}
      use Ecto.Schema
    end
  end
end
