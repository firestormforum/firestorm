defmodule FirestormWeb.Web.Api.V1.PreviewControllerTest do
  use FirestormWeb.ConnCase
  alias FirestormWeb.Markdown

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  test "POST /", %{conn: conn} do
    post = %{
      "body" => "this is **neat**",
    }
    conn = post conn, "/api/v1/preview", post: post
    response = json_response(conn, 201)["data"]
    assert response["html"] == Markdown.render(post["body"])
  end
end
