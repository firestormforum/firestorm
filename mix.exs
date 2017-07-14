defmodule FirestormWeb.Mixfile do
  use Mix.Project

  def project do
    [app: :firestorm_web,
     version: "0.0.1",
     elixir: "~> 1.4",
     elixirc_paths: elixirc_paths(Mix.env),
     compilers: [:phoenix, :gettext] ++ Mix.compilers,
     start_permanent: Mix.env == :prod,
     aliases: aliases(),
     deps: deps()]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [mod: {FirestormWeb.Application, []},
     extra_applications: [:logger, :runtime_tools]]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_),     do: ["lib"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:phoenix, "~> 1.3.0-rc", override: true},
      {:phoenix_pubsub, "~> 1.0"},
      {:phoenix_ecto, "~> 3.2"},
      {:postgrex, ">= 0.0.0"},
      {:phoenix_html, "~> 2.6"},
      {:phoenix_live_reload, "~> 1.0", only: :dev},
      {:gettext, "~> 0.11"},
      {:cowboy, "~> 1.0"},
      {:ueberauth, "0.4.0"},
      {:ueberauth_github, "0.4.1"},
      {:ueberauth_identity, "~> 0.2.3"},
      {:oauth2, "0.9.0"},
      {:earmark, "~> 1.2.2"},
      {:exmoji, "~> 0.2.2"},
      {:scrivener_ecto, "~> 1.2.2"},
      {:scrivener_html, "~> 1.7"},
      {:bamboo, github: "thoughtbot/bamboo"},
      {:ecto_autoslug_field, "~> 0.3.0"},
      {:uuid, "~> 1.1"},
      {:timex, "~> 3.1.15"},
      {:comeonin, "~> 3.0.2"},
      {:exgravatar, "~> 2.0.0"},
      {:bodyguard, "~> 2.0.1"},
      {:html_sanitize_ex, "~> 1.2"},
      {:pryin, "~> 1.0"},

      # TEST DEPENDENCIES
      {:wallaby, "~> 0.17.0", only: [:test]},
      {:credo, "~> 0.8", only: [:dev, :test], runtime: false},

      # OVERRIDES
      # We can remove this override once we're on exmoji > 0.2.2
      {:poison, "~> 3.0", override: true},
    ]
  end

  # Aliases are shortcuts or tasks specific to the current project.
  # For example, to create, migrate and run the seeds file at once:
  #
  #     $ mix ecto.setup
  #
  # See the documentation for `Mix` for more info on aliases.
  defp aliases do
    ["ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],
     "ecto.reset": ["ecto.drop", "ecto.setup"],
     "test": ["ecto.drop", "ecto.create --quiet", "ecto.migrate", "test"]]
  end
end
