defmodule FirestormWeb.Web.Api.V1.UploadSignatureView do
  use FirestormWeb.Web, :view

  def render("show.json", %{upload_signature: upload_signature}) do
    %{data: render_one(upload_signature, __MODULE__, "upload_signature.json")}
  end

  def render("upload_signature.json", %{upload_signature: upload_signature}) do
    upload_signature
    |> Map.take([
      :key,
      :date,
      :content_type,
      :acl,
      :success_action_status,
      :action,
      :aws_access_key_id,
      :credential,
      :policy,
      :signature
    ])
  end

  def render("error.json", %{errors: errors}) do
    %{errors: %{detail: errors}}
  end
end
