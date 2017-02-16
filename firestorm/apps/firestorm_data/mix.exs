defmodule FirestormData.Mixfile do
  use Mix.Project

  def project do
    [
      app: :firestorm_data,
      version: "0.1.0",
      build_path: "../../_build",
      config_path: "../../config/config.exs",
      deps_path: "../../deps",
      lockfile: "../../mix.lock",
      elixir: "~> 1.4",
      build_embedded: Mix.env == :prod,
      start_permanent: Mix.env == :prod,
      aliases: aliases(),
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [
        :logger
      ],
      mod: {FirestormData.Application, []}
    ]
  end

  defp deps do
    [
      {:ecto, "~> 2.1.2"},
      {:postgrex, ">= 0.0.0"},
      {:arbor, "~> 1.0.3"},
      {:ecto_autoslug_field, "~> 0.2"},
    ]
  end

  defp aliases do
    [
      "test": ["ecto.drop --quiet", "ecto.create --quiet", "ecto.migrate", "test"]
    ]
  end
end
