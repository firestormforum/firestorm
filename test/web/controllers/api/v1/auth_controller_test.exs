defmodule FirestormWeb.Web.Api.V1.AuthControllerTest do
  use FirestormWeb.Web.ConnCase
  alias FirestormWeb.Markdown

  test "POST /identity", %{conn: conn} do
    user = %{
      "username" => "admin",
      "password" => "password"
    }
    conn = post conn, "/api/v1/auth/identity", user: user
    response = json_response(conn, 200)
    assert response["data"]["api_token"]
  end
end
