# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :firestorm_web,
  ecto_repos: [FirestormWeb.Repo]

# Should we send instrumentation to PryIn?
config :firestorm_web, use_pryin: false

# Configures the endpoint
config :firestorm_web, FirestormWeb.Web.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "Q1bBtoEs8F2+MsssVU8zHXvhJBVFDI/4EFEINldDXBlVsBkNr30gi4WIGNwds+YO",
  render_errors: [view: FirestormWeb.Web.ErrorView, accepts: ~w(html json)],
  pubsub: [name: FirestormWeb.PubSub,
           adapter: Phoenix.PubSub.PG2],
  instrumenters: []

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Configures Ueberauth for OAuth stuff
config :ueberauth, Ueberauth,
  providers: [
    github: {Ueberauth.Strategy.Github, [default_scope: ""]},
    identity: {Ueberauth.Strategy.Identity, [
      callback_methods: ["POST"]
    ]},
  ]

config :ueberauth, Ueberauth.Strategy.Github.OAuth,
  client_id: System.get_env("GITHUB_CLIENT_ID"),
  client_secret: System.get_env("GITHUB_CLIENT_SECRET")

config :firestorm_web, FirestormWeb.Mailer,
  adapter: Bamboo.SendGridAdapter,
  api_key: System.get_env("SENDGRID_API_KEY")

# Configuration for AWS integration
config :firestorm_web, :aws,
  access_key_id: System.get_env("AWS_ACCESS_KEY_ID"),
  secret_access_key: System.get_env("AWS_SECRET_ACCESS_KEY"),
  bucket: System.get_env("AWS_S3_BUCKET"),
  region: System.get_env("AWS_S3_REGION")

config :scrivener_html,
  routes_helper: FirestormWeb.Web.Router.Helpers,
  view_style: :bootstrap_v4

config :pryin,
  api_key: System.get_env("PRYIN_API_KEY"),
  otp_app: :firestorm,
  enabled: false,
  env: :dev

config :firestorm_web, FirestormWeb.Repo,
  loggers: [PryIn.EctoLogger, Ecto.LogEntry]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"
