defmodule FirestormWeb.UploadRequest do
  use Ecto.Schema

  alias Ecto.Changeset

  embedded_schema do
    field :filename, :string
    field :mimetype, :string
  end

  def changeset(req, params) do
    req
    |> Changeset.cast(params, [:filename, :mimetype])
    |> Changeset.validate_required([:filename, :mimetype])
  end
end

defmodule FirestormWeb.Web.Api.V1.UploadSignatureController do
  use FirestormWeb.Web, :controller
  alias FirestormWeb.UploadRequest

  def create(conn, %{"upload" => upload_params}) do
    case handle_upload(upload_params) do
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

  def handle_upload(upload_params) do
    changeset = UploadRequest.changeset(%UploadRequest{}, upload_params)

    if changeset.valid? do
      request = Ecto.Changeset.apply_changes(changeset)

      signature =
        request.filename
        |> file_path
        |> FirestormWeb.AWS.UploadSignature.signature(request.mimetype)

      {:ok, signature}
    else
      {:error, "There was a problem with your upload signature request"}
    end
  end

  defp file_path(filename) do
    uuid = UUID.uuid4()
    "uploads/#{uuid}/#{filename}"
  end
end
