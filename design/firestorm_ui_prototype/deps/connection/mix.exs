defmodule Connection.Mixfile do
  use Mix.Project

  @version "1.0.4"

  def project do
    [app: :connection,
     version: @version,
     elixir: "~> 1.2",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     description: description(),
     package: package(),
     docs: docs(),
     deps: deps()]
  end

  def application do
    [applications: []]
  end

  defp deps() do
    [{:ex_doc, "~> 0.13", only: :dev}]
  end

  defp docs do
    [source_url: "https://github.com/fishcakez/connection",
     source_ref: "v#{@version}",
     main: Connection]
  end

  defp description do
    """
    Connection behaviour for connection processes
    """
  end

  defp package do
    %{licenses: ["Apache 2.0"],
      links: %{"Github" => "https://github.com/fishcakez/connection"}}
  end
end
