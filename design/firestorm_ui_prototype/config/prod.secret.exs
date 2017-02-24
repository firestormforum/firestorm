use Mix.Config

# In this file, we keep production configuration that
# you likely want to automate and keep it away from
# your version control system.
#
# You should document the content of this
# file or create a script for recreating it, since it's
# kept out of version control and might be hard to recover
# or recreate for your teammates (or you later on).
config :firestorm_ui_prototype, FirestormUiPrototype.Endpoint,
  secret_key_base: "CJCdvGdQ0NCB+i7xGoKT4OVfZ8PLFsitpttzYGgVl9W8q2PjKD4ml1kvyyLxqbVk"

# Configure your database
config :firestorm_ui_prototype, FirestormUiPrototype.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "postgres",
  password: "postgres",
  database: "firestorm_ui_prototype_prod",
  pool_size: 20
