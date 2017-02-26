defmodule Firestorm.Mixfile do
  use Mix.Project

  def project do
    [
      apps_path: "apps",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      deps: deps(),
      aliases: aliases(),
    ]
  end

  defp deps do
    [
      {:credo, "~> 0.6.1", only: [:dev, :test]},
    ]
  end

  defp aliases do
    [
      "ecto.setup": ["ecto.create", "ecto.migrate", "ecto.seed"],
      "ecto.seed": ["run apps/firestorm_data/priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      "test": ["ecto.drop", "ecto.create --quiet", "ecto.migrate", "test"]
    ]
  end
end
