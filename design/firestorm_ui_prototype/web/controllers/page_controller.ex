defmodule FirestormUiPrototype.PageController do
  use FirestormUiPrototype.Web, :controller
  alias DataModelPlayground.{Category, Post, Thread, User}

  def index(conn, _params) do
    render conn, "index.html"
  end

  def home(conn, _params) do
    { elixir, otp } = mock_categories()
    threads = %{
      otp: mock_threads()
    }

    render conn, "home.html", elixir: elixir, otp: otp, threads: threads
  end

  def mock_categories() do
    elixir = %Category{title: "Elixir"}
    otp = %Category{title: "OTP"}

    { elixir, otp }
  end

  def mock_users() do
    adamdill = %User{username: "adamdill"}
    knewter = %User{username: "knewter"}
    franzejr = %User{username: "franzejr"}

    { adamdill, knewter, franzejr}
  end

  def mock_threads() do
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

  def mock_post() do
    %Post{}
  end
end
