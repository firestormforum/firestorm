defmodule FirestormWeb.Web.Api.V1.UploadSignatureController do
  use FirestormWeb.Web, :controller

  def create(conn, %{"upload" => %{"filename" => filename, "mimetype" => mimetype}}) do
    case FirestormWeb.Uploads.sign(filename, mimetype) do
      {:ok, signature} ->
        conn
        |> put_status(201)
        |> render("show.json", upload_signature: signature)

      {:error, errors} ->
        conn
        |> put_status(:unprocessable_entity)
        |> render("error.json", errors: errors)
    end
  end
end
