defmodule FirestormWeb.InboundControllerTest do
  @moduledoc false

  use FirestormWeb.ConnCase
  import FirestormWeb.DataHelper

  setup do
    :ok = Ecto.Adapters.SQL.Sandbox.checkout(Repo)
  end

  describe "POST /inbound/sendgrid" do
    setup [:create_users, :create_categories_and_threads]

    test "creates post on corresponding thread", %{users: %{knewter: knewter}, categories: %{elixir: elixir}, conn: conn} do
      conn =
        conn
        |> post(inbound_path(conn, :sendgrid), %{foo: "bar"})

      assert json_response(conn, 200) =~ "ok"
    end
  end

  def inbound_email do
    %{
      "SPF" => "pass",
      "attachments" => "0",
      "charsets" => "{\"to\":\"UTF-8\",\"html\":\"UTF-8\",\"subject\":\"UTF-8\",\"from\":\"UTF-8\",\"text\":\"UTF-8\"}",
      "dkim" => "{@gmail.com : pass}",
      "envelope" => "{\"to\":[\"josh@notifier.firestormforum.org\"],\"from\":\"josh.rubyist@gmail.com\"}",
      "from" => "Josh Adams <josh.rubyist@gmail.com>",
      "headers" => "Received: by mx0043p1mdw1.sendgrid.net with SMTP id hZOsSzHOPr Wed, 15 Mar 2017 18:34:02 +0000 (UTC)\nReceived: from mail-ot0-f180.google.com (mail-ot0-f180.google.com [74.125.82.180]) by mx0043p1mdw1.sendgrid.net (Postfix) with ESMTPS id 6F27C4A1329 for <josh@notifier.firestormforum.org>; Wed, 15 Mar 2017 18:34:02 +0000 (UTC)\nReceived: by mail-ot0-f180.google.com with SMTP id i1so28956781ota.3 for <josh@notifier.firestormforum.org>; Wed, 15 Mar 2017 11:34:02 -0700 (PDT)\nDKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed; d=gmail.com; s=20161025; h=mime-version:from:date:message-id:subject:to; bh=GwnSAhdI/u8PHC2HFrt77xzyVvP7DT+u4xmRa8rHHoY=; b=Y/Nqpsb49EiAESNhJ0eqKQDcORBW4LP2u2k9apYtQuUvPh732+gvu/Ue9fV46C4VP8 8YAnlZpBIBgeK3pXvIel2DW8cUSRotfXP6sY0vcoehVev2kJeaCYPJ/mOwLqYikbUddh gwB2CDYhD84DQGbVt1wSLfMzILpVqyiiGzHiRJUJeShuP26qsFhGifY9BUnw9ULgCKa0 NFmvfToFPrSlJU4BPtI6JjuFvjLSULJjAJJrHxfDZtCkboyV022dvp9++wxLtJLIizqg ZN8h8UHo5Mk4DVsiaP6wVVUWzB0Hn3LxJT9usqndZj2J2tSvYv7TTlQCKuIqqnQ9HbiZ 17Kw==\nX-Google-DKIM-Signature: v=1; a=rsa-sha256; c=relaxed/relaxed; d=1e100.net; s=20161025; h=x-gm-message-state:mime-version:from:date:message-id:subject:to; bh=GwnSAhdI/u8PHC2HFrt77xzyVvP7DT+u4xmRa8rHHoY=; b=F61cfB12rrT5PFmXkRm30gi/WFL818m1IYCS5y0Fg2GNdtwAoXr0aBBhbxodXqbIHZ fhSHfOaEP6ghdlz0BP0T4iQd060xkpIDZaJ7ei4CaSK2kMZjUPo02QBEHz6KLxOO64uw vD+bIVAzm+w65YK1b8j6g5aQ2K51mG1Ws5SrUGD61ytclY8s0mxIv+yC21Q+Ottkt4CR RojHZZwc1Gkvz90A0AfoZ4Ic/xQEpcsz4W3UoWK90VeL6Z8dbueP0/51ODTH5EtZKMkT IyavilQPGhbqRJTlzIM8DGgeeca1SgtHO+LvnhOggyLEDk2tYz9ir3uQCycNlS/1l2nM HvAQ==\nX-Gm-Message-State: AFeK/H2qEtipNG451j/+PHPcQw2JE3imo/aNif1F1vYas8j6uYeD8pBcpGjy/ZcW5YPtzwy74LzVLnmz6LNR5g==\nX-Received: by 10.202.84.14 with SMTP id i14mr1762454oib.14.1489602841909; Wed, 15 Mar 2017 11:34:01 -0700 (PDT)\nMIME-Version: 1.0\nReceived: by 10.157.60.246 with HTTP; Wed, 15 Mar 2017 11:34:01 -0700 (PDT)\nFrom: Josh Adams <josh.rubyist@gmail.com>\nDate: Wed, 15 Mar 2017 13:34:01 -0500\nMessage-ID: <CAA1-O0xpOdkYR9C7fiVkJVeDrTL=4NQrBdyz-YCxLB9kr6MwKA@mail.gmail.com>\nSubject: test\nTo: josh@notifier.firestormforum.org\nContent-Type: multipart/alternative; boundary=001a113ad0403fa15d054ac93097\n",
      "html" => "<div dir=\"ltr\">testing<br clear=\"all\"><div><br></div>-- <br><div class=\"gmail_signature\" data-smartmail=\"gmail_signature\">Josh Adams<br></div>\r\n</div>\n",
      "sender_ip" => "74.125.82.180",
      "subject" => "test",
      "text" => "testing\r\n\r\n-- \r\nJosh Adams\n",
      "to" => "josh@notifier.firestormforum.org"
    }
  end
end
