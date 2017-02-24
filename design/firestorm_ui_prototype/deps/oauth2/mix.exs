defmodule OAuth2.Mixfile do
  use Mix.Project

  @version "0.9.0"

  def project do
    [app: :oauth2,
     name: "OAuth2",
     version: @version,
     elixir: "~> 1.2",
     deps: deps(),
     package: package(),
     description: description(),
     docs: docs(),
     elixirc_paths: elixirc_paths(Mix.env),
     test_coverage: [tool: ExCoveralls],
     preferred_cli_env: [
       "coveralls": :test,
       "coveralls.detail": :test,
       "docs": :dev
     ]]
  end

  def application do
    [applications: [:logger, :hackney],
     env: [serializers: %{"application/json" => Poison}]]
  end

  defp deps do
    [{:hackney, "~> 1.6"},

     # Test dependencies
     {:poison, "~> 3.0", only: :test},
     {:bypass, "~> 0.5", only: :test},
     {:excoveralls, "~> 0.5", only: :test},

     # Docs dependencies
     {:ex_doc, "~> 0.14", only: :dev}]
  end

  defp description do
    "An Elixir OAuth 2.0 Client Library"
  end

  defp docs do
    [extras: ["README.md"],
     main: "readme",
     source_ref: "v#{@version}",
     source_url: "https://github.com/scrogson/oauth2"]
  end

  defp package do
    [files: ["lib", "mix.exs", "README.md", "LICENSE"],
     maintainers: ["Sonny Scroggin"],
     licenses: ["MIT"],
     links: %{github: "https://github.com/scrogson/oauth2"}]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]
end
