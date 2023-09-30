![Firestorm](./firestorm-logo.png)

### An open-source forum engine, with an Elixir+Phoenix backend and an Elm frontend.

#### A community-funded project from [DailyDrip](https://www.dailydrip.com).

[![Build Status](https://semaphoreci.com/api/v1/dailydrip/firestorm/branches/master/badge.svg)](https://semaphoreci.com/dailydrip/firestorm)

This is the Firestorm Forum project. It's presently at version 0.10, nearly ready
for a 1.0 release. You can view the running forum at
<http://forum.firestormforum.org>.

## Usage
To use Firestorm Forum, you'll need to have Elixir and Phoenix installed on your machine. Once you have those installed, you can follow these steps:

1. Clone the repository: git clone https://github.com/dailydrip/firestorm.git
2. Navigate to the project directory: cd firestorm
3. Install dependencies: mix deps.get
4. Create and migrate the database: mix ecto.setup
5. Start the Phoenix server: mix phx.server

You should now be able to access the forum at http://localhost:4000.

### How to Download
To download Firestorm Forum, you can follow these steps:

1. Go to the GitHub repository.
2. Click on the green "Code" button and select "Download ZIP".
3. Extract the downloaded ZIP file to your desired location.

### What are the system requirements for running Firestorm Forum?

The system requirements for running Firestorm Forum are Elixir and Phoenix. You can find the specific version requirements in the `mix.exs` file. Additionally, you'll need a database such as PostgreSQL or MySQL. The specific version requirements for the database will also be listed in the `mix.exs` file.

### What database options are available for use with Firestorm Forum?

Firestorm Forum supports multiple databases, including PostgreSQL, MySQL, and SQLite. You can configure the database in the `config/dev.exs` and `config/prod.exs` files. By default, Firestorm Forum is configured to use PostgreSQL.

### How can I configure the database for Firestorm Forum?

To configure the database for Firestorm Forum, you can modify the `config/dev.exs` and `config/prod.exs` files. Here's an example configuration for PostgreSQL:

config :firestorm_forum, FirestormForum.Repo,
  adapter: Ecto.Adapters.Postgres,
  username: "postgres",
  password: "postgres",
  database: "firestorm_forum_dev",
  hostname: "localhost",
  pool_size: 10

You can replace the values for `username`, `password`, `database`, and `hostname` with your own values. If you're using a different database, you'll need to change the adapter value to the appropriate adapter. For example, if you're using MySQL, you would change `adapter: Ecto.Adapters.Postgres` to `adapter: Ecto.Adapters.MySQL`.

### How can I run the unit tests for Firestorm Forum?

To run the unit tests for Firestorm Forum, you can use the `mix test` command. Here's an example of how to run the tests:

1. Open a terminal window and navigate to the project directory.
2. Run the command `mix test`.

This will run all of the unit tests for the project. You should see the output of the tests in the terminal window. If any tests fail, you'll see an error message indicating which test failed and why.

## Clients

### Elm

https://github.com/dailydrip/firestorm_elm

### Vue

https://github.com/dailydrip/firestorm_vue

### Flutter

https://github.com/dailydrip/firestorm_flutter

### React Native

https://github.com/dailydrip/firestorm_react_native


## Patrons

This project was funded by [a
Kickstarter](https://www.kickstarter.com/projects/1003377429/firestorm-an-open-source-forum-in-phoenix-from-eli).

All of the patrons that made it possible are listed in [the PATRONS file](PATRONS.md).


## License

Firestorm is [MIT Licensed](./LICENSE).
