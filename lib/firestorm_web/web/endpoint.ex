defmodule FirestormWeb.Web.Endpoint do
  use Phoenix.Endpoint, otp_app: :firestorm_web

  if Application.get_env(:firestorm_web, :sql_sandbox) do
    plug Phoenix.Ecto.SQL.Sandbox
  end

  socket "/socket", FirestormWeb.Web.UserSocket

  # Serve at "/" the static files from "priv/static" directory.
  #
  # You should set gzip to true if you are running phoenix.digest
  # when deploying your static files in production.
  plug Plug.Static,
    at: "/", from: :firestorm_web, gzip: false,
    only: ~w(css fonts images js favicon.ico robots.txt android-icon-192x192.png android-icon-144x144.png android-icon-96x96.png android-icon-72x72.png android-icon-48x48.png android-icon-36x36.png manifest.json favicon.ico favicon-96x96.png favicon-32x32.png favicon-16x16.png ms-icon-70x70.png ms-icon-310x310.png ms-icon-150x150.png ms-icon-144x144.png apple-icon-precomposed.png apple-icon.png apple-icon-76x76.png apple-icon-72x72.png apple-icon-60x60.png apple-icon-57x57.png apple-icon-180x180.png apple-icon-152x152.png apple-icon-144x144.png apple-icon-120x120.png apple-icon-114x114.png)

  # Code reloading can be explicitly enabled under the
  # :code_reloader configuration of your endpoint.
  if code_reloading? do
    socket "/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket
    plug Phoenix.LiveReloader
    plug Phoenix.CodeReloader
  end

  plug Plug.RequestId
  plug Plug.Logger

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Poison

  plug Plug.MethodOverride
  plug Plug.Head

  # The session will be stored in the cookie and signed,
  # this means its contents can be read but not tampered with.
  # Set :encryption_salt if you would also like to encrypt it.
  plug Plug.Session,
    store: :cookie,
    key: "_firestorm_web_key",
    signing_salt: "YhYEFZld"

  plug PryIn.Plug
  plug FirestormWeb.Web.Router

  @doc """
  Dynamically loads configuration from the system environment
  on startup.

  It receives the endpoint configuration from the config files
  and must return the updated configuration.
  """
  def load_from_system_env(config) do
    port = System.get_env("PORT") || raise "expected the PORT environment variable to be set"
    {:ok, Keyword.put(config, :http, [:inet6, port: port])}
  end
end
