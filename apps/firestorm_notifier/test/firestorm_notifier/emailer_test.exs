defmodule FirestormNotifier.EmailerTest do
  use ExUnit.Case
  import Bamboo.Email

  test "creating an email" do
    email =
      new_email()
      |> to("josh@dailydrip.com")
      |> from("adam@dailydrip.com")
      |> subject("Testing Bamboo")
      |> html_body("<h1>Hey</h1><p>Check out this email or else.</p>")
      |> text_body("# Hey\n\nCheck out this email or else.")

    assert email.to == "josh@dailydrip.com"
    assert email.from == "adam@dailydrip.com"
    assert email.subject == "Testing Bamboo"
    assert email.html_body =~ ~r/Check/
    assert email.text_body =~ ~r/Check/
  end
end
