defmodule FirestormWeb.Mixfile do
  use Mix.Project

  def project do
    [app: :firestorm_web,
     version: "0.0.1",
     build_path: "../../_build",
     config_path: "../../config/config.exs",
     deps_path: "../../deps",
     lockfile: "../../mix.lock",
     elixir: "~> 1.3",
     elixirc_paths: elixirc_paths(Mix.env),
     compilers: [:phoenix, :gettext] ++ Mix.compilers,
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  # Configuration for the OTP application.
  #
  # Type `mix help compile.app` for more information.
  def application do
    [
      mod: {FirestormWeb, []},
      applications: [
        :phoenix,
        :phoenix_pubsub,
        :phoenix_html,
        :cowboy,
        :logger,
        :gettext,
        :ueberauth,
        :ueberauth_github,
      ]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "web", "test/support"]
  defp elixirc_paths(_),     do: ["lib", "web"]

  # Specifies your project dependencies.
  #
  # Type `mix help deps` for examples and options.
  defp deps do
    [
      {:phoenix, "~> 1.2.0"},
      {:phoenix_pubsub, "~> 1.0"},
      {:phoenix_html, "~> 2.6"},
      {:phoenix_haml, "~> 0.2.1"},

      # FIXME: I don't want to depend on ecto here. I'd rather define changesets
      # for our comands and our own protocol implementation for
      # Phoenix.HTML.FormData for those!
      {:phoenix_ecto, "~> 3.2.2"},

      {:phoenix_live_reload, "~> 1.0", only: :dev},
      {:gettext, "~> 0.11"},
      {:cowboy, "~> 1.0"},
      {:ueberauth, "~> 0.4"},
      {:ueberauth_github, "~> 0.4"},

      {:firestorm_data, in_umbrella: true},
    ]
  end
end
