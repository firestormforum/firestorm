# CommonJS require definition

Simple [CommonJS](http://wiki.commonjs.org/wiki/Modules/1.1) `require()` definition.

## Usage

Include on top of your file and register modules with `require.register(name, fn)`


```html
<script src="require.js"></script>
<script>
require.register("module", function(exports, require, module) {
  // Expose `count` externally.
  exports.count = 42;
});
console.log(require("module").count);
</script>
```

## API

* `require(name)` — loads registered module and returns its `exports`.
* `require.register(name, fn)` — registers new module. `fn` callback receives `exports, require, module`.
* `require.list()` — lists all registered modules.

## License

The MIT License (MIT)

Copyright (c) 2013 Paul Miller (http://paulmillr.com/)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the “Software”), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
