babel-brunch
===========
Brunch plugin using [babel](https://github.com/babel/babel) to turn latest
ECMAScript standard code into vanilla ES5 with no runtime required.

All the `.js` files in your project will be run through the babel compiler,
except those it is configured to ignore, unless you use the `pattern` option.

Additionally, starting Brunch 2.7.3, babel-brunch will also compile NPM dependencies.

Installation
------------
`npm install --save babel-brunch`

Usage
-----

- Presets for the latest ECMAScript standard are *included by default*. If you want to use them, just install the plugin. No configuration required.
- To add **React** preset:
    - Execute `npm install --save-dev babel-preset-react`, then adjust the `presets` option in `brunch-config.js`:
    - `plugins: {babel: {presets: ['es2015', 'es2016', 'react']}}`
- Default behavior is to handle `js` files which are not dependencies and `jsx` files if you enable React preset.

Configuration
-------------
Set [babel options](https://babeljs.io/docs/usage/options) in your brunch
config (such as `brunch-config.js`) except for `filename` and `sourceMap`
which are handled internally.

This plugin uses, by default, the
[es2015](https://babeljs.io/docs/plugins/preset-es2015/) and
[es2016](https://babeljs.io/docs/plugins/preset-es2016/) presets. To use no
preset, set the configuration option to an empty array.

Additionally, you can set an `ignore` value to specify which `.js` files in
your project should not be compiled by babel. By default, `ignore` is set to
`/^(bower_components|vendor)/`.

You can also set `pattern` to a regular expression that will match the file
paths you want compiled by babel, which will override the standard behavior of
compiling every `.js` file.

```js
plugins: {
  babel: {
    presets: ['es2015', 'es2016'],
    ignore: [
      /^(bower_components|vendor)/,
      'app/legacyES5Code/**/*'
    ],
    pattern: /\.(es6|jsx)$/
  }
}
```

Change Log
----------
[See release notes page on GitHub](https://github.com/babel/babel-brunch/releases)

License
-------
[ISC](https://raw.github.com/babel/babel-brunch/master/LICENSE)
