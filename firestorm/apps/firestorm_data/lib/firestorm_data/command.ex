defmodule FirestormData.Command do
  defmacro __using__(_opts) do
    quote do
      alias FirestormData.{Repo, User, Category, Thread, Post}
      import Ecto.Query, only: [from: 2]
    end
  end
end
