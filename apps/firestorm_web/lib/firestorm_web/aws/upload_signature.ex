defmodule FirestormWeb.AWS.UploadSignature do
  @service "s3"
  @aws_request "aws4_request"

  def signature(filename, mimetype) do
    policy = policy(filename, mimetype)

    %{
      key: filename,
      date: get_date(),
      content_type: mimetype,
      acl: "public-read",
      success_action_status: "201",
      action: bucket_url(),
      aws_access_key_id: aws_config()[:access_key_id],
      policy: policy,
      credential: credential(),
      signature: sign(policy)
    }
  end

  def get_date() do
    datetime = Timex.now
    {:ok, t} = Timex.format(datetime, "%Y%m%d", :strftime)
    t
  end

  defp credential() do
    credential(aws_config()[:access_key_id], get_date())
  end

  defp credential(key, date) do
    key <> "/" <> date <> "/" <> region() <> "/" <> @service <> "/" <> @aws_request
  end

  defp policy(key, mimetype, expire_after_min \\ 60) do
    %{
      expiration: min_from_now(expire_after_min),
      conditions: [
        %{bucket: bucket_name()},
        %{acl: "public-read"},
        ["starts-with", "$Content-Type", mimetype],
        ["starts-with", "$key", key],
        %{success_action_status: "201"}
      ]
    }
    |> Poison.encode!()
    |> Base.encode64()
  end

  defp min_from_now(minutes) do
    import Timex

    now()
    |> shift(minutes: minutes)
    |> format!("{ISO:Extended:Z}")
  end

  defp sign(policy) do
    :sha
    |> :crypto.hmac(secret_access_key(), policy)
    |> Base.encode64()
  end

  defp bucket_name() do
    aws_config()[:bucket]
  end

  defp region() do
    aws_config()[:region]
  end

  defp secret_access_key() do
    aws_config()[:secret_access_key]
  end

  defp bucket_url() do
    "https://s3-#{region()}.amazonaws.com/#{bucket_name()}"
  end

  defp aws_config() do
    Application.get_env(:firestorm_web, :aws)
  end
end
