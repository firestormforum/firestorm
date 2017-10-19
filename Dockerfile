FROM elixir:1.5.2
ENV DEBIAN_FRONTEND=noninteractive

RUN mix local.hex --force
RUN mix local.rebar --force
RUN mix archive.install https://github.com/phoenixframework/archives/raw/master/phoenix_new.ez --force

RUN apt-get install -y -q nodejs

RUN mkdir /firestorm
ADD . /firestorm
WORKDIR /firestorm
