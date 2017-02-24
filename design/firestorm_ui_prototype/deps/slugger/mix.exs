defmodule Slugger.Mixfile do
  use Mix.Project

  def project do
    [app: :slugger,
     name: "Slugger",
     source_url: "https://github.com/h4cc/slugger",
     version: "0.1.0",
     elixir: "~> 1.0",
     description: description,
     package: package,
     deps: deps]
  end

  defp description do
    """
    The library Slugger can generate slugs from given strings that can be used in URLs or file names.
    """
  end

  defp package do
    [# These are the default files included in the package
     files: ["config", "test", "lib", "priv", "mix.exs", "README*", "readme*", "LICENSE*", "license*"],
     maintainers: ["Julius Beckmann"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/h4cc/slugger",
              "Docs" => "http://hexdocs.pm/slugger/"}]
  end

  def application do
    [applications: []]
  end

  defp deps() do
    [
      {:earmark, "~> 0.1", only: :dev},
      {:ex_doc, "~> 0.6", only: :dev},
      {:benchfella, "~> 0.3.0", only: :dev},

      {:excheck, "~> 0.3", only: :test},
      {:triq, github: "krestenkrab/triq", only: :test}
    ]
  end
end
