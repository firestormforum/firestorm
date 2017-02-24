# since-app-start

Get time passed since you've started node.js process.

## Usage

```javascript
// 1. Set global.appStartTime before you load any node.js modules.
global[Symbol.for('start-time')] = Date.now()

// 2. The following line would return a prepared for output string.
require('since-app-start').addEntry('Loaded modules')
```

## License

[The MIT License](https://raw.githubusercontent.com/paulmillr/mit/master/README.md)
