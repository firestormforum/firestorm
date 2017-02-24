defmodule EctoAutoslugField.Mixfile do
  use Mix.Project

  @version "0.2.1"
  @url "https://github.com/sobolevn/ecto_autoslug_field"

  def project do
    [
      app: :ecto_autoslug_field,
      version: @version,
      elixir: "~> 1.2",
      deps: deps(),

      # Hex:
      docs: docs(),
      description: description(),
      package: package(),
      source_url: @url,
      homepage_url: @url,

      # Test coverage:
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        "coveralls": :test,
        "coveralls.detail": :test,
        "coveralls.post": :test,
        "coveralls.html": :test,
      ],
   ]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    [
      # Database:
      {:ecto, "~> 2.0"},

      # Slugs:
      {:slugger, "~> 0.1"},

      # Testing:
      {:excoveralls, "~> 0.5", only: :test},
      {:credo, "~> 0.6", only: [:dev, :test]},

      # Documentation:
      {:ex_doc, "~> 0.14", only: :dev},
    ]
  end

  defp description do
    "Autoslug field for Ecto."
  end

  defp docs do
    [extras: ["README.md"], main: "readme"]
  end

  defp package do
    [
      maintainers: ["Nikita Sobolev"],
      licenses: ["MIT"],
      links: %{
        "GitHub" => @url,
      },
      files: ~w(mix.exs README.md lib),
    ]
  end
end
