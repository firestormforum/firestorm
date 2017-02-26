# OTP Application

This will outline the design of the OTP Application.

The actual forum is just an OTP application without any external interfaces
provided. It can be interacted with via [the Phoenix application](./PHOENIX.md)
for remote purposes, or via its API for other applications living in our system.

At present, it consists of `FirestormData` but it will also include a
notification system and it will be interfaced with via a Slack app as well once
the core forum is operational and sufficient functionality has been provided.
