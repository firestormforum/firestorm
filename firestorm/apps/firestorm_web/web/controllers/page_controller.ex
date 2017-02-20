defmodule FirestormWeb.PageController do
  use FirestormWeb.Web, :controller
  alias FirestormData.{Category, Post, Thread, User}

  def index(conn, _params) do
    current_user = Plug.Conn.get_session conn, "current_user"
    render conn, "index.html", current_user: current_user
  end

  def home(conn, _params) do
    { elixir, otp, elm, elm_mdl } = mock_categories()
    threads = %{
      otp: mock_otp_threads(),
      elm_mdl: mock_elm_mdl_threads()
    }

    render conn, "home.html", elixir: elixir, otp: otp, threads: threads, elm: elm, elm_mdl: elm_mdl
  end

  def thread(conn, _params) do
    render conn, "thread.html"
  end

  def mock_categories() do
    elixir = %Category{title: "Elixir"}
    otp = %Category{title: "OTP"}
    elm = %Category{title: "Elm"}
    elm_mdl = %Category{title: "elm-mdl"}

    { elixir, otp, elm, elm_mdl }
  end

  def mock_users() do
    adamdill = %User{username: "adamdill"}
    knewter = %User{username: "knewter"}
    franzejr = %User{username: "franzejr"}

    { adamdill, knewter, franzejr}
  end

  def mock_otp_threads() do
    { adamdill, knewter, franzejr } = mock_users()

    how_do_i_otp = %Thread{posts: [mock_post(), mock_post()], title: "How do I OTP?"}
    do_you_even_process = %Thread{posts: [mock_post(), mock_post()], title: "Do you even Process?"}
    dhtml = %Thread{posts: [mock_post(), mock_post(), mock_post()], title: "DHTML!"}

    [
      { how_do_i_otp, adamdill },
      { do_you_even_process, knewter },
      { dhtml, franzejr }
    ]
  end

  def mock_elm_mdl_threads() do
    { adamdill, knewter, _franzejr } = mock_users()

    elm_mdl_textfields = %Thread{posts: [mock_post(), mock_post()], title: "elm_mdl textfields"}
    extracting = %Thread{posts: [mock_post(), mock_post()], title: "Extracting Modules, Nested Records, and Parameterized Types"}

    [
      { elm_mdl_textfields, knewter },
      { extracting, adamdill },
    ]
  end

  def mock_post() do
    %Post{}
  end
end
