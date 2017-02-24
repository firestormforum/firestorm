'use strict';

const CleanCSS = require('clean-css');

class CleanCSSMinifier {
  constructor(config) {
    this.options = config && config.plugins && config.plugins.cleancss || {};
  }

  optimize(params) {
    const data = params.data;
    const path = params.path;

    let error, optimized;

    try {
      if (this.options.ignored && this.options.ignored.test(path)) {
        // ignored file path: return non minified
        return Promise.resolve(data);
      }
    } catch (e) {
      return Promise.reject(`error checking ignored files to minify ${e}`);
    }

    try {
      optimized = new CleanCSS(this.options).minify(data).styles;
    } catch (_error) {
      error = `CSS minify failed on ${path}: ${_error}`;
    } finally {
      if (error) return Promise.reject(error);

      return Promise.resolve(optimized || data);
    }
  }
}

CleanCSSMinifier.prototype.brunchPlugin = true;
CleanCSSMinifier.prototype.type = 'stylesheet';

module.exports = CleanCSSMinifier;
