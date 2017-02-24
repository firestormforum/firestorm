var express = require('express');
var http = require('http');
var sysPath = require('path');
var slashes = require('connect-slashes');
var serveStatic = require('serve-static');

var startServer = function(options, callback) {
  // Specify default options.
  if (typeof options === 'function') {
    callback = options;
    options = null;
  }
  if (options == null) options = {};
  if (options.path == null) options.path = '.';
  if (options.port == null) options.port = 8000;
  if (options.hostname == null) options.hostname = 'localhost';
  if (options.base == null) options.base = '';
  if (options.indexPath == null) options.indexPath = 'index.html';
  if (options.noCors == null) options.noCors = false;
  if (options.stripSlashes == null) options.stripSlashes = false;
  if (options.noPushState == null) options.noPushState = false;
  if (options.noLog == null) options.noLog = false;
  if (callback == null) callback = Function.prototype;

  var rootPath = sysPath.resolve(options.path);
  var sendFileOptions = {root: rootPath};
  var address = 'http://' + options.hostname + ':' + options.port;
  var app = express();

  // Send cross-origin resource sharing enabling header.
  if (!options.noCors) {
    app.use(function(request, response, next) {
      response.header('Cache-Control', 'no-cache');
      response.header('Access-Control-Allow-Origin', '*');
      next();
    });
  }

  // Route all static files to http paths.
  app.use(options.base, serveStatic(rootPath));

  // Redirect requests that include a trailing slash.
  if (options.stripSlashes) {
    app.use(slashes(false));
  }

  // Route base requests to `options.indexPath`
  app.all('/', function(request, response) {
    response.sendFile(options.indexPath, sendFileOptions);
  });

  // Route all non-existent files to `index.html`
  if (!options.noPushState) {
    app.all('' + options.base + '/*', function(request, response) {
      response.sendFile(options.indexPath, sendFileOptions);
    });
  }

  // Wrap xpress app with node.js server in order to have stuff like server.stop() etc.
  var server = http.createServer(app);
  server.on('error', function (e) {
    if (e.code === 'EADDRINUSE') {
      console.log('ERROR: Another process already listening on ' + address);
      process.exit();
    }
  });
  server.timeout = 2000;
  server.listen(options.port, options.hostname, function(error) {
    if (!options.noLog) {
      console.log('Application started on ' + address);
    }
    callback(error, options);
  });
  return server;
};

module.exports = startServer;
