'use strict';

const babel = require('babel-core');
const anymatch = require('anymatch');
const resolve = require('path').resolve;

const reIg = /^(bower_components|vendor)/;
const reJsx = /\.(es6|jsx|js)$/;

const prettySyntaxError = (err) => {
  if (err._babel && err instanceof SyntaxError) {
    return `${err.name}: ${err.message}\n${err.codeFrame}`;
  } else {
    return err;
  }
};

class BabelCompiler {
  constructor(config) {
    if (!config) config = {};
    const options = config.plugins &&
      (config.plugins.babel || config.plugins.ES6to5) || {};
    const opts = Object.keys(options).reduce((obj, key) => {
      if (key !== 'sourceMap' && key !== 'ignore') {
        obj[key] = options[key];
      }
      return obj;
    }, {});
    opts.sourceMap = !!config.sourceMaps;
    if (!opts.presets) opts.presets = ['es2015', 'es2016'];
    if (!opts.plugins) opts.plugins = [];
    const origPresets = opts.presets;
    // this is needed so that babel can locate presets when compiling node_modules
    const mapOption = type => data => {
      const resolvePath = name => (
        resolve(config.paths.root, 'node_modules', `babel-${type}-${name}`)
      );
      if (typeof data === 'string') return resolvePath(data);
      return [resolvePath(data[0]), data[1]];
    };
    const mappedPresets = opts.presets.map(mapOption('preset'));
    const mappedPlugins = opts.plugins.map(mapOption('plugin'));
    opts.presets = mappedPresets;
    opts.plugins = mappedPlugins;
    if (origPresets.indexOf('react') !== -1) this.pattern = reJsx;
    if (opts.presets.length === 0) delete opts.presets;
    if (opts.plugins.length === 0) delete opts.plugins;
    if (opts.pattern) {
      this.pattern = opts.pattern;
      delete opts.pattern;
    }
    this.isIgnored = anymatch(options.ignore || reIg);
    this.options = opts;
  }

  compile(params) {
    if (this.isIgnored(params.path)) return Promise.resolve(params);
    this.options.filename = params.path;
    this.options.sourceFileName = params.path;

    return new Promise((resolve, reject) => {
      let compiled;

      try {
        compiled = babel.transform(params.data, this.options);
      } catch (error) {
        reject(prettySyntaxError(error));
        return;
      }

      const result = {data: compiled.code || compiled};

      // Concatenation is broken by trailing comments in files, which occur
      // frequently when comment nodes are lost in the AST from babel.
      result.data += '\n';

      if (compiled.map) result.map = JSON.stringify(compiled.map);
      resolve(result);
    });
  }
}

BabelCompiler.prototype.brunchPlugin = true;
BabelCompiler.prototype.type = 'javascript';
BabelCompiler.prototype.extension = 'js';

module.exports = BabelCompiler;
