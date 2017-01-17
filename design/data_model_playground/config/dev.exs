use Mix.Config

config :data_model_playground, DataModelPlayground.Repo,
  adapter: Ecto.Adapters.Postgres,
  database: "firestorm_data_model_playground_dev",
  username: "postgres",
  hostname: "localhost",
  port: "5432"
