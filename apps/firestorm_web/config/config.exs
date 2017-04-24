# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

config :phoenix, :template_engines,
  haml: PhoenixHaml.Engine

config :firestorm_web, ecto_repos: []

# To make acceptance testing easier, we have a flag we can flip to let us get
# the current user from a cookie rather than the session store. We don't do that
# by default.
config :firestorm_web, :get_session_from_cookies, false

# Configures the endpoint
config :firestorm_web, FirestormWeb.Web.Endpoint,
  http: [port: 4000],
  url: [
    host: "localhost"
  ],
  secret_key_base: "+WIPnl6jAtKueGGXBI5TY+76Skp2AGuqMfvgW3Rg6oT4Txk+Xc8Qruz48mohK8ds",
  render_errors: [
    view: FirestormWeb.Web.ErrorView,
    accepts: ~w(html json)
  ],
  pubsub: [
    name: FirestormWeb.Web.PubSub,
    adapter: Phoenix.PubSub.PG2
  ]

# Configuration for AWS integration
config :firestorm_web, :aws,
  access_key_id: System.get_env("AWS_ACCESS_KEY_ID"),
  secret_access_key: System.get_env("AWS_SECRET_ACCESS_KEY"),
  bucket: System.get_env("AWS_S3_BUCKET"),
  region: System.get_env("AWS_S3_REGION")

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Configures Ueberauth for OAuth stuff
config :ueberauth, Ueberauth,
  providers: [
    github: {Ueberauth.Strategy.Github, [default_scope: ""]}
  ]

config :ueberauth, Ueberauth.Strategy.Github.OAuth,
  client_id: System.get_env("GITHUB_CLIENT_ID"),
  client_secret: System.get_env("GITHUB_CLIENT_SECRET")

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
