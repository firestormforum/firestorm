use Mix.Config

config :phoenix_integration,
  endpoint: FirestormWeb.Endpoint

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :firestorm_web, FirestormWeb.Endpoint,
  http: [port: 4001],
  server: false

# Print only warnings and errors during test
config :logger, level: :warn
