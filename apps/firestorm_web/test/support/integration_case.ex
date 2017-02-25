defmodule FirestormWeb.IntegrationCase do
  use ExUnit.CaseTemplate

  using do
    quote do
      use FirestormWeb.ConnCase
      use PhoenixIntegration
      import FirestormWeb.DataHelper
    end
  end
end
