defmodule FirestormWeb.Navigation.Defaults do
  defmacro __using__(_) do
    quote do
      def back(_other, _assigns), do: nil

      defoverridable [back: 2]
    end
  end
end
