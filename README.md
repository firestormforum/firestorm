![Firestorm](./assets/static/images/firestorm-logo.png)
### An open-source forum engine, with an Elixir+Phoenix backend and an Elm frontend.
#### A community-funded project from [DailyDrip](https://www.dailydrip.com).

[![Build Status](https://semaphoreci.com/api/v1/dailydrip/xx-firestorm/branches/master/badge.svg)](https://semaphoreci.com/dailydrip/xx-firestorm)
[![Deps Status](https://beta.hexfaktor.org/badge/all/github/dailydrip/firestorm.svg)](https://beta.hexfaktor.org/github/dailydrip/firestorm)

This is the Firestorm Forum project.  We've just reached the "initial
dogfooding" phase and you can view the first deployed version at
<http://firestorm-dogfood.herokuapp.com>.

## Patrons

This project was funded by [a
Kickstarter](https://www.kickstarter.com/projects/1003377429/firestorm-an-open-source-forum-in-phoenix-from-eli).

All of the patrons that made it possible are listed in [the PATRONS file](PATRONS.md).

## Development

Here are some basic steps to get Firestorm running

```shell
git clone git@github.com:dailydrip/firestorm.git
cd firestorm/
# Set the following environment variables
# AWS_ACCESS_KEY_ID=TKTK
# AWS_SECRET_ACCESS_KEY=TKTK
# AWS_S3_BUCKET=TKTK
# AWS_S3_REGION=TKTK
# GITHUB_CLIENT_ID=TKTK
# GITHUB_CLIENT_SECRET=TKTK
# SENDGRID_API_KEY=TKTK
# config postgres in config/database.yml
# start postgres
mix deps.get
mix ecto.create
mix ecto.migrate
cd assets && npm install
cd ../
mix phx.server
```

## Features

Here are the planned features and their status:

- [x] [Continuous Integration](https://semaphoreci.com/dailydrip/firestorm)
  - We're using Semaphore CI for this
- [ ] Continuous Deployment
  - With releases, via [distillery](https://github.com/bitwalker/distillery)
  - For now, we've got Semaphore pushing to
    <http://forum.firestormforum.org> after each successful build of
    master.
- [x] OAuth
  - At present, we only support GitHub. We'll support a lot more services in the
    future, but until we're happy with the state of the rest of the dogfooding
    session we'll stick with GitHub.
- [ ] Email (first class!)
  - The goal here is to be a fantastic replacement for Google Groups
- [x] Forums
- [x] Topics
  - [ ] Stickies
- [x] Posts
- [ ] Personal Messaging
- [ ] Mentions
- [ ] Documentation
  - [ ] Example Project
  - [ ] Quickstart / Setup Guide
  - [ ] Docs for systems integrating with firestorm within the same BEAM vm
- [ ] Tags
- [ ] Search
- [ ] Slack Integration
- [ ] Plugin System
- [ ] Custom Profile Fields

## Quick Fixes

- [x] Fix timestamps, use moment.js and ISO format / time_abbr helper
- [x] Style pagination on user show
- [-] navigation enhancements
  - [x] threads i'm participating in
    - [ ] drill down by category (for category show sidebar)
  - [x] threads i'm watching
    - [ ] drill down by category (for category show sidebar)
  - [ ] recently viewed
    - [ ] drill down by category (for category show sidebar)
  - [x] users
  - [x] your profile
- [ ] quote button
- [x] replying via email
- [ ] reactions / emoji picker
- [ ] tags

## Configuration

### S3

Attachments are stored on S3. Consequently, you will need to create an S3 bucket
to store them, as well as an API user that can write to it and list all of your
buckets, and configure the bucket for CORS.

You also need to set 4 environment variables:

```sh
export AWS_ACCESS_KEY_ID=XXXX
export AWS_SECRET_ACCESS_KEY=XXXX
export AWS_S3_BUCKET=XXXX
export AWS_S3_REGION=XXXX
```

#### CORS configuration

Here's an example CORS configuration:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<CORSConfiguration xmlns="http://s3.amazonaws.com/doc/2006-03-01/">
  <CORSRule>
    <AllowedOrigin>http://localhost:4000</AllowedOrigin>
    <AllowedMethod>POST</AllowedMethod>
    <AllowedHeader>*</AllowedHeader>
  </CORSRule>
  <CORSRule>
    <AllowedOrigin>http://firestorm-dogfood.herokuapp.com</AllowedOrigin>
    <AllowedMethod>POST</AllowedMethod>
    <AllowedHeader>*</AllowedHeader>
  </CORSRule>
</CORSConfiguration>
```

## License

Firestorm is [MIT Licensed](./LICENSE).
