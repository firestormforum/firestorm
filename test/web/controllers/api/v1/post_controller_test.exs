defmodule FirestormWeb.Web.Api.V1.PostControllerTest do
  use FirestormWeb.Web.ConnCase
  alias FirestormWeb.{
    Markdown,
    Forums
  }

  test "POST /", %{conn: conn} do
    {:ok, elixir} = Forums.create_category(%{title: "Elixir"})
    {:ok, bob} =
      Forums.login_or_register_from_identity(%{username: "bob", password: "password"})
    {:ok, otp_is_cool} = Forums.create_thread(elixir, bob, %{title: "OTP is cool", body: "Don't you think?"})

    post = %{
      "body" => "this is **neat**",
    }

    conn =
      conn
      |> put_req_header("authorization", "Bearer #{bob.api_token}")
      |> post("/api/v1/posts", post: post, thread_id: otp_is_cool.id)

    response = json_response(conn, 201)["data"]
    assert response["body"] == Markdown.render(post["body"])
  end
end
