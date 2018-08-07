defmodule FirestormWeb.Mixfile do
  use Mix.Project

  def project do
    [
      app: :firestorm_web,
      version: "0.10.0",
      elixir: "~> 1.6",
      elixirc_paths: elixirc_paths(Mix.env()),
      compilers: [:phoenix, :gettext] ++ Mix.compilers(),
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps()
    ]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [mod: {FirestormWeb.Application, []}, extra_applications: [:logger, :runtime_tools]]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:phoenix, "~> 1.3.3", override: true},
      {:phoenix_pubsub, "~> 1.0"},
      {:phoenix_ecto, "~> 3.3"},
      {:postgrex, ">= 0.3.5"},
      {:phoenix_html, "~> 2.11"},
      {:phoenix_live_reload, "~> 1.1.5", only: :dev},
      {:gettext, "~> 0.11"},
      {:cowboy, "~> 1.0"},
      {:ueberauth, "~> 0.5"},
      {:ueberauth_github, "~> 0.7"},
      {:ueberauth_identity, "~> 0.2.3"},
      {:oauth2, "0.9.2"},
      {:earmark, "~> 1.2.2"},
      {:exmoji, "~> 0.2.2"},
      {:scrivener_ecto, "~> 1.3"},
      {:scrivener_html, "~> 1.7"},
      {:bamboo, github: "thoughtbot/bamboo"},
      {:ecto_autoslug_field, "~> 0.5"},
      {:uuid, "~> 1.1"},
      {:timex, "~> 3.3"},
      {:comeonin, "~> 4.1"},
      {:bcrypt_elixir, "~> 1.0"},
      {:exgravatar, "~> 2.0.0"},
      {:bodyguard, "~> 2.2"},
      {:html_sanitize_ex, "~> 1.2"},
      {:pryin, "~> 1.0"},
      {:oembed, "~> 0.2"},
      {:lru_cache, "~> 0.1.3"},
      {:cors_plug, "~> 1.5"},
      {:ex_admin, github: "smpallen99/ex_admin"},
      {:httpoison, "~> 1.2.0", override: true},

      # TEST DEPENDENCIES
      {:wallaby, "~> 0.20", only: [:test]},
      {:credo, "~> 0.8", only: [:dev, :test], runtime: false},
      {:dogma, "~> 0.1", only: [:dev]},

      # DEV DEPENDENCIES
      {:dialyxir, "~> 0.5", only: [:dev], runtime: false},

      # OVERRIDES
      # We can remove this override once we're on exmoji > 0.2.2
      {:poison, "~> 4.0", override: true}
    ]
  end

  # Aliases are shortcuts or tasks specific to the current project.
  # For example, to create, migrate and run the seeds file at once:
  #
  #     $ mix ecto.setup
  #
  # See the documentation for `Mix` for more info on aliases.
  defp aliases do
    [
      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
      "ecto.reset": ["ecto.drop", "ecto.setup"],
      test: ["ecto.drop", "ecto.create --quiet", "ecto.migrate", "test"]
    ]
  end
end
