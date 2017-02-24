connect-slashes
===============

Trailing slash redirect middleware for Connect and Express.js. Useful for creating canonical urls in your Node.js applications.

## Installation

```
$ npm install connect-slashes
```

## Usage

```javascript
var connect = require("connect")
  , slashes = require("connect-slashes");

connect()
  .use(connect.logger())
  .use(connect.static())
  .use(slashes())
  .listen(3000);
```

Alternatively, you can pass `false` as the first argument to `.slashes()` in order to remove trailing slashes instead of appending them:

```javascript
.use(slashes(false));
```

## Additional settings

You can also pass a second argument with an options object. For example, if an application is behind a reverse proxy server that removes part of the URL (a base_path) before proxying to the application, then the `base` can be specified with an option:

```javascript
.use(slashes(true, { base: "/blog" })); // prepends a base url to the redirect
```

By default, all redirects are using the 301 Moved Permanently header. You can change this behavior by passing in the optional `code` option:

```javascript
.use(slashes(true, { code: 302 })); // 302 Temporary redirects
```

You can also set additional headers to the redirect response with the `headers` option:

```javascript
.use(slashes(true, { headers: { "Cache-Control": "public" } }));
```

## Notes

1. Only GET requests will be redirected (to avoid losing POST/PUT data)
2. This middleware will append or remove a trailing slash to all request urls. This includes filenames (/app.css => /app.css/), so it may break your static files. Make sure to `.use()` this middleware only after the `connect.static()` middleware.

## LICENSE

MIT
