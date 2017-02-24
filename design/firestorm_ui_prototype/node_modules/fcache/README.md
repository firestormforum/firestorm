# fcache

Standartized `fs.readfile` caching mechanism for incremental build systems.

## Usage

```javascript
// Your watcher
const fcache = require('fcache');
watcher.on('change', path => {
  fcache.updateCache(path).then(data => {
    // Do some stuff.
  });
});

// Later, in your plugin
fcache.readFile(path).then(data => {
  // Would use cached version.
});

```

## Supported Node.js versions

fcache | Node.js
------ | -------
~0.1   | > 0.8
~0.2   | > 4.0
