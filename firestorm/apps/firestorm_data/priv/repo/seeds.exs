alias FirestormData.{Category, Post, Thread, User, Repo}

# Categories
elixir = %Category{title: "Elixir"} |> Repo.insert!
elm = %Category{title: "Elm"} |> Repo.insert!
otp = %Category{title: "OTP", parent: elixir} |> Repo.insert!
elm_mdl = %Category{title: "elm-mdl", parent: elm} |> Repo.insert!


# Users
adamdill = %User{username: "adamdill"} |> Repo.insert!
knewter = %User{username: "knewter"} |> Repo.insert!
franzejr = %User{username: "franzejr"} |> Repo.insert!

# Threads
## OTP
how_do_i_otp =
  %Thread{title: "How do I OTP?", category: otp}
  |> Repo.insert!

do_you_even_process =
  %Thread{title: "Do you even Process?", category: otp}
  |> Repo.insert!

dhtml =
  %Thread{title: "DHTML!", category: otp}
  |> Repo.insert!

## elm-mdl
elm_mdl_textfields =
  %Thread{title: "elm_mdl textfields", category: elm_mdl}
  |> Repo.insert!

extracting =
  %Thread{title: "Extracting Modules, Nested Records, and Parameterized Types", category: elm_mdl}
  |> Repo.insert!


# Posts
how_do_i_otp_fp =
  %Post{user: adamdill, body: "asdfsdaf", thread: how_do_i_otp}
  |> Repo.insert!

do_you_even_process_fp =
  %Post{user: knewter, body: "asdfsdaf", thread: do_you_even_process}
  |> Repo.insert!

dhtml_fp =
  %Post{user: franzejr, body: "asdfsdaf", thread: dhtml}
  |> Repo.insert!

elm_mdl_textfields_fp =
  %Post{user: knewter, body: "asdfsdaf", thread: elm_mdl_textfields}
  |> Repo.insert!

extracting_fp =
  %Post{user: knewter, body: "asdfsdaf", thread: extracting}
  |> Repo.insert!
