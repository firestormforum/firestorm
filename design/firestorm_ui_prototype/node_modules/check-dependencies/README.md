# check-dependencies

> Checks if currently installed npm/bower dependencies are installed in the exact same versions that are specified in package.json/bower.json

[![Travis build](https://img.shields.io/travis/mgol/check-dependencies.svg?style=flat-square)](https://travis-ci.org/mgol/check-dependencies)
[![AppVeyor build](https://img.shields.io/appveyor/mgol/check-dependencies.svg?style=flat-square)](https://ci.appveyor.com/project/mgol/check-dependencies)
[![Version](https://img.shields.io/npm/v/check-dependencies.svg?style=flat-square)](http://npm.im/check-dependencies)
[![Downloads](https://img.shields.io/npm/dm/check-dependencies.svg?style=flat-square)](http://npm-stat.com/charts.html?package=check-dependencies)
[![MIT License](https://img.shields.io/npm/l/check-dependencies.svg?style=flat-square)](http://opensource.org/licenses/MIT)

## Installation

To install the package and add it to your `package.json`, invoke:

```shell
npm install check-dependencies --save-dev
```

## Rationale

When dependencies are changed in `package.json` (or `bower.json`), whether it's a version bump or a new package, one can forget to invoke `npm install` (or `bower install`) and continue using the application, possibly encountering errors caused by obsolete package versions. To avoid it, use the `check-dependencies` module at the top of the entry point of your application; it will inform about not up-to-date setup and optionally install the dependencies.

Another option would be to always invoke `npm install` (or `bower install`) at the top of the main file but it can be slow and `check-dependencies` is fast.

## Usage

Once the package has been installed, it may be used via:

### CLI

```bash
$ check-dependencies
```

All options from the [API](#api) except `log` and `error` can be passed to the CLI, example:

```bash
$ check-dependencies --verbose --package-manager bower --scope-list dependencies
```

Options accepting array values in the API (like [`scopeList`](#scopelist)) should have each value passed individually, example:
```bash
$ check-dependencies --scope-list dependencies --scope-list devDependencies
```

### API

```js
require('check-dependencies')(config, callback);
```
where `callback` is invoked upon completion and `config` is a configuration object.

`callback` is invoked with the object containing fields:
```js
{
    status: number,      // 0 if successful, 1 otherwise
    depsWereOk: boolean, // true if dependencies were already satisfied
    log: array,          // array of logged messages
    error: array,        // array of logged errors
}
```

The function returns a promise so passing a callback is not necessary; instead you can do:
```js
require('check-dependencies')(config)
    .then(function (output) {
        /* handle output */
    });
```
The promise should never fail.

There is a synchronous alternative -- the following code:
```js
var output = require('check-dependencies').sync(config);
```
will assign to `output` the same object that would otherwise be passed to the `callback` in the asynchronous scenario.

The `config` object may have the following fields:

#### packageManager

Package manager to check against. Possible values: `'npm'`, `'bower'`. (Note: for `bower` you need to have the `bower` package installed either globally or locally in the same project in which you use `check-dependencies`).

Type: `string`

Default: `'npm'`

#### packageDir

Path to the directory containing `package.json` or `bower.json`.

Type: `string`

Default: the closest directory containing `package.json` or `bower.json` (depending on `packageManager` specified) when going up the tree, starting from the current one

#### onlySpecified

Ensures all installed dependencies are specified in `package.json` or `bower.json`.

NOTE: Don't use this option with npm 3.0.0 or newer as it deduplicates the file dependency tree by default so `check-dependencies` will think many modules are excessive whereas in fact they will not.

Type: `boolean`

Default: `false`

#### install

Installs packages if they don't match. With the `onlySpecified` option enabled prune excessive packages as well.

Type: `boolean`

Default: `false`

#### scopeList

The list of keys in `package.json` or `bower.json` where to look for package names & versions.

Type: `array`

Default: `['dependencies', 'devDependencies']`

#### optionalScopeList

The list of keys in `package.json` or `bower.json` where to look for *optional* package names & versions. An optional package is not required to be installed but if it's installed, it's supposed to match the specified version range.

This list is also consulted when using `onlySpecified: true`.

Type: `array`

Default: `['optionalDependencies']`

#### checkCustomPackageNames

By default, check-dependencies will skip version check for custom package names, but will still check to see if they are installed.  For example:

```js
    "dependencies": {
      "specialSemver059": "semver#0.5.9"
    }
```

If checkCustomPackageNames is enabled, check-dependencies will parse the version number (after the hash) for custom package names and check it against the version of the installed package of the same name.

Type: `boolean`

Default: `false`

#### checkGitUrls

By default, check-dependencies will skip version check for packages whose version contains the full repository path.  For example:

```js
    "dependencies": {
      "semver": "https://github.com/npm/node-semver.git#0.5.9"
    }
```

If checkGitUrls is enabled, check-dependencies will parse the version number (after the path to the git repository and the hash) and check it against the version of the installed package.

Type: `boolean`

Default: `false`

#### verbose

Prints messages to the console.

Type: `boolean`

Default: `false`

#### log

A function logging debug messages (applies only if `verbose: true`).

Type: `function`

Default: `console.log.bind(console)`

#### error

A function logging error messages (applies only if `verbose: true`).

Type: `function`

Default: `console.error.bind(console)`

## Usage Examples

The most basic usage:
```js
require('check-dependencies')(callback);
```
This will check packages' versions and report an error to `callback` if packages' versions are mismatched.

The following:
```js
require('check-dependencies')({
    install: true,
    verbose: true,
}, callback);
```
will install mismatched ones and call `callback`.

The following two examples:
```js
require('check-dependencies')(callback);
require('check-dependencies')({}, callback);
```
behave in the same way - `callback` is invoked upon completion; if there was an error, it's passed as a parameter to `callback`.

## Supported Node.js versions
This project aims to support all Node.js LTS versions in the "active" phase (see [LTS README](https://github.com/nodejs/LTS/blob/master/README.md) for more details) as well as the latest stable Node.js.

## Contributing
In lieu of a formal styleguide, take care to maintain the existing coding style. Add unit tests for any new or changed functionality. Lint and test your code using `npm test`.

## License
Copyright (c) 2014 Michał Gołębiowski. Licensed under the MIT license.
