defmodule FirestormWeb.Web.Api.V1.UploadSignatureControllerTest do
  use FirestormWeb.Web.ConnCase

  test "POST /", %{conn: conn} do
    upload = %{
      "filename" => "image.jpg",
      "mimetype" => "image/jpeg"
    }
    conn = post conn, "/api/v1/upload_signature", upload: upload
    response = json_response(conn, 201)["data"]
    assert response
  end
end
