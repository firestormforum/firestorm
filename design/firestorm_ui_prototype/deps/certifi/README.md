certifi
=====

This Erlang library contains a CA bundle that you can reference in your Erlang
application. This is useful for systems that do not have CA bundles that
Erlang can find itself, or where a uniform set of CAs is valuable.

This an Erlang specific port of [certifi](http://certifi.io/). The CA bundle
is derived from Mozilla's canonical set.

Usage
-----

```erlang
Cacerts = certifi:cacerts(),
Ssloptions = [{verify, verify_peer},
              {depth, 99},
              {cacerts, Cacerts}],
ssl:connect( "example.com", 443, Ssloptions] ).
```
Build
-----

    $ rebar3 compile

Regenerate the certificates
---------------------------

    $ ./mkcert.escript
