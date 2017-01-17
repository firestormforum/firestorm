use Mix.Config

config :data_model_playground, DataModelPlayground.Repo,
  adapter: Ecto.Adapters.Postgres,
  pool: Ecto.Adapters.SQL.Sandbox,
  database: "firestorm_data_model_playground_test",
  username: "postgres",
  hostname: "localhost",
  port: "5432"
