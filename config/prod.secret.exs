use Mix.Config

# In this file, we keep production configuration that
# you likely want to automate and keep it away from
# your version control system.
#
# You should document the content of this
# file or create a script for recreating it, since it's
# kept out of version control and might be hard to recover
# or recreate for your teammates (or you later on).
config :firestorm_web, FirestormWeb.Web.Endpoint,
  secret_key_base: "WWB1EdJl5bJrrhAxdCuXva0dk1gb6RRGr8jqj6x7ozLDzZjB+7VxQgg49K6jOZI3"

# Configure your database
config :firestorm_web, FirestormWeb.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "postgres",
  password: "postgres",
  database: "firestorm_web_prod",
  pool_size: 15
