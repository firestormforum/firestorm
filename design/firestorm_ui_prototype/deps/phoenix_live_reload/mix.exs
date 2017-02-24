defmodule PhoenixLiveReload.Mixfile do
  use Mix.Project

  @version "1.0.8"

  def project do
    [app: :phoenix_live_reload,
     version: @version,
     elixir: "~> 1.0.2 or ~> 1.1",
     deps: deps(),

     # Hex
     description: "Provides live-reload functionality for Phoenix",
     package: package(),

     # Docs
     name: "Phoenix Live-Reload",
     docs: [source_ref: "v#{@version}",
            source_url: "https://github.com/phoenixframework/phoenix_live_reload"]]
  end

  defp package do
    [maintainers: ["Chris McCord"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/phoenixframework/phoenix_live_reload"}]
  end

  def application do
    [applications: [:logger, :phoenix, :fs]]
  end

  defp deps do
    [{:phoenix, "~> 1.0 or ~> 1.2-rc"},
     {:ex_doc, "~> 0.11", only: :docs},
     {:earmark, ">= 0.0.0", only: :docs},
     {:fs, "~> 0.9.1"}]
  end
end
