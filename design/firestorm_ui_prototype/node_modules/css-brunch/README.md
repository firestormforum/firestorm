## css-brunch
Adds CSS support to [brunch](http://brunch.io).

## Usage
Install the plugin via npm with `npm install --save css-brunch`.

Or, do manual install:

* Add `"css-brunch": "x.y.z"` to `package.json` of your brunch app.
  Pick a plugin version that corresponds to your minor (y) brunch version.
* If you want to use git version of plugin, add
`"css-brunch": "git+ssh://git@github.com:brunch/css-brunch.git"`.

### CSS Modules
Starting Brunch `<unreleased>`, you can use CSS Modules with css-brunch. To enable it, change your config to:

```javascript
module.exports = {
  // ...
  plugins: {
    css: {
      cssModules: true
    }
  }
};
```

Then, author your styles like you normally would:

```css
.title {
  font-size: 32px;
}
```

And reference CSS class names by requiring the specific style into your javascript:

```javascript
var style = require('./title.css');

<h1 className={style.title}>Yo</h1>
```

Note: enabling `cssModules` does so for every stylesheet in your project, so it's all-or-nothing. Even the files you don't require will be transformed into CSS modules (aka will have obfuscated class names, like turn `.title` into `._title_fdphn_1`).

## License

The MIT License (MIT)

Copyright (c) 2012-2013 Paul Miller (http://paulmillr.com)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
