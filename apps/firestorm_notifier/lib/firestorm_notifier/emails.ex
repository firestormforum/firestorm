defmodule FirestormNotifier.Emails do
  import Bamboo.Email

  def welcome_email do
    new_email
    |> to("josh@dailydrip.com")
    |> from("josh@dailydrip.com")
    |> subject("Welcome!!!")
    |> html_body("<strong>Welcome</strong>")
    |> text_body("welcome")
  end
end
