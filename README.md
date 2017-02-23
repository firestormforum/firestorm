# Firestorm
## An open-source forum engine, with an Elixir+Phoenix backend and an Elm frontend.
[![Build Status](https://semaphoreci.com/api/v1/dailydrip/firestorm/branches/master/badge.svg)](https://semaphoreci.com/dailydrip/firestorm)

> A project from [DailyDrip](https://www.dailydrip.com).

This is the Firestorm Forum project.  It's wrapping up the initial design phase
presently.  [The design document](design/README.md) is a good place to start
reading.

## Patrons

This project was funded by [a
Kickstarter](https://www.kickstarter.com/projects/1003377429/firestorm-an-open-source-forum-in-phoenix-from-eli).

All of the patrons that made it possible are listed in [the PATRONS
file](PATRONS.md).

## Code

You can find the current state of affairs living [in the firestorm
directory](./firestorm).

## Features

At present, we're wrapping up [the initial design phase](design/README.md). Having
said that, here are the planned features and their status:

- [ ] Continuous Integration
  - We'll use Semaphore CI for this
- [ ] Continuous Deployment
  - With releases, via [distillery](https://github.com/bitwalker/distillery)
- [ ] OAuth
- [ ] Email (first class!)
  - The goal here is to be a fantastic replacement for Google Groups
- [ ] Forums
- [ ] Topics
  - [ ] Stickies
- [ ] Posts
- [ ] Personal Messaging
- [ ] Tags
- [ ] Search
- [ ] Slack Integration
- [ ] Plugin System
