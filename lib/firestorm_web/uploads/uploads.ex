defmodule FirestormWeb.Uploads do
  alias FirestormWeb.Uploads.AWS.UploadSignature

  def sign(filename, mimetype) do
    signature =
      filename
      |> file_path()
      |> UploadSignature.signature(mimetype)

    {:ok, signature}
  end

  defp file_path(filename) do
    uuid = UUID.uuid4()
    "uploads/#{uuid}/#{filename}"
  end
end
