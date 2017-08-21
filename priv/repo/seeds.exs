# Script for populating the database. You can run it as:
#
#     mix run priv/repo/seeds.exs
#
# Inside the script, you can read and write to any of your
# repositories directly:
#
#     FirestormWeb.Repo.insert!(%FirestormWeb.SomeSchema{})
#
# We recommend using the bang functions (`insert!`, `update!`
# and so on) as they will fail if something goes wrong.
alias FirestormWeb.Forums

IO.puts "~~~~~ INITIALIZING FIRESTORM SEEDS ~~~~~"
IO.puts "-- CREATING USERS --"

IO.puts "---- Bob Vladbob ----"
{:ok, bob} =
  Forums.register_user(%{
    username: "bob",
    email: "bob@example.com",
    name: "Bob Vladbob",
    password: "password"
  })

IO.puts "---- Alice McStinkerton ----"
{:ok, alice} =
  Forums.register_user(%{
    username: "alice",
    email: "alice@example.com",
    name: "Alice McStinkerton",
    password: "password"
  })

IO.puts "-- CREATING CATEGORIES --"
IO.puts "---- Elixir ----"
{:ok, elixir} =
  Forums.create_category(%{title: "Elixir"})

IO.puts "---- Elm ----"
{:ok, elm} =
  Forums.create_category(%{title: "Elm"})

IO.puts "-- CREATING THREADS --"
IO.puts "---- OTP is cool ----"
{:ok, otp_is_cool} =
  Forums.create_thread(elixir, bob, %{title: "OTP is cool", body: "Don't you think?"})

{:ok, elm_mdl} =
  Forums.create_thread(elm, alice, %{title: "elm-mdl", body: "What's the story on this library these days?"})
IO.puts "-- CREATING POSTS --"
{:ok, otp_post} = Forums.create_post(otp_is_cool, alice, %{body: "I really do"})
