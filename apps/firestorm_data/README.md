![Firestorm](../firestorm_web/web/static/assets/images/firestorm-logo.png)
## FirestormData

This is the data layer for the [Firestorm Forum](../../README.md). It contains:

- Ecto schemas
- Commands for interacting with the data layer

In general, the idea is that any interactions with the data layer should happen
via commands, and the `Repo` itself should not be interacted with from outside of
this app. In practice, [`firestorm_web`](../firestorm_web) is still doing a few
things with the `Repo` because the commands layer is not completely built out
yet.
