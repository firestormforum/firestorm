defmodule FirestormData.UnitCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      use ExUnit.Case
      import FirestormData.DataHelpers
    end
  end
end
