# universal-path

Cross-platform universal node.js `path` module replacement that works better with Windows.

## Usage

```javascript
const sysPath = require('universal-path');

// Two additional helper methods.
sysPath.slashes(path)   // Converts win32 path to unit path.
sysPath.unslashes(path) // Converts unix path to win32 path.

// Methods from node.js `path`.
sysPath.isAbsolute()
sysPath.basename()
sysPath.sep()

// Methods from node.js `path`; `slashes` is executed on the output.
sysPath.normalize()
sysPath.relative()
sysPath.resolve()
sysPath.join()
sysPath.dirname()
```

## License

MIT

Copyright (c) 2016 Paul Miller (http://paulmillr.com)
