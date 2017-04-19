defmodule FirestormWeb.Mixfile do
  use Mix.Project

  def project do
    [app: :firestorm_web,
     version: "0.0.1",
     build_path: "../../_build",
     config_path: "../../config/config.exs",
     deps_path: "../../deps",
     lockfile: "../../mix.lock",
     elixir: "~> 1.4",
     elixirc_paths: elixirc_paths(Mix.env),
     compilers: [:phoenix, :gettext] ++ Mix.compilers,
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     aliases: aliases(),
     deps: deps()]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [
      mod: {FirestormWeb.Application, []},
      extra_applications: [:logger]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_),     do: ["lib"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:phoenix, "~> 1.3.0-rc.1", override: true},
      {:phoenix_pubsub, "~> 1.0"},
      {:phoenix_html, "~> 2.6"},
      {:phoenix_haml, "~> 0.2"},
      {:phoenix_ecto, "~> 3.2"},
      {:gettext, "~> 0.11"},
      {:cowboy, "~> 1.0"},
      {:ueberauth, "~> 0.4"},
      {:ueberauth_github, "~> 0.4"},
      {:earmark, "~> 1.2.0"},
      {:timex, "~> 3.0"},
      {:fs, "~> 2.12", override: true},
      {:exmoji, "~> 0.2.2"},
      {:poison, "~> 3.1.0", override: true},
      {:cors_plug, "~> 1.2"},
      {:bamboo, github: "thoughtbot/bamboo"},
      {:html_sanitize_ex, github: "marvelm/html_sanitize_ex"},

      # UMBRELLA
      {:firestorm_data, in_umbrella: true},

      # DEV/TEST
      {:credo, "~> 0.6.1", only: [:dev, :test]},
      {:phoenix_integration, "~> 0.2", only: :test},
      {:wallaby, github: "keathley/wallaby", only: [:dev, :test]},
      {:ex_machina, "~> 2.0", only: [:dev, :test]},
      {:faker, "~> 0.7", only: [:dev, :test]},
      {:phoenix_live_reload, "~> 1.0.8", only: :dev},
    ]
  end

  defp aliases do
    [
      "test": ["ecto.drop --quiet", "ecto.create --quiet", "ecto.migrate", "test"],
    ]
  end
end
