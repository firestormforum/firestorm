use Mix.Config

config :phoenix_integration,
  endpoint: FirestormWeb.Web.Endpoint

# We don't run a server during test. If one is required,
# you can enable the server option below.
config :firestorm_web, FirestormWeb.Web.Endpoint,
  http: [port: 4001],
  server: true

# We check this config key in FirestormWeb.Endpoint to determine whether to use
# a SQL sandbox or not when spinning up the Phoenix app
config :firestorm_web, :sql_sandbox, true

# To make acceptance testing easier, we have a flag we can flip to let us get
# the current user from a cookie rather than the session store.
config :firestorm_web, :get_session_from_cookies, true

# Print only warnings and errors during test
config :logger, level: :warn
