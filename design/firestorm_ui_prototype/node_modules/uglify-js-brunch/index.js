'use strict';

const uglify = require('uglify-js');

class UglifyJSOptimizer {
  constructor(config) {
    if (config == null) config = {};
    this.options = Object.assign({}, config.plugins && config.plugins.uglify || {});
    this.options.fromString = true;
    this.options.sourceMaps = !!config.sourceMaps;
  }

  optimize(args) {
    let error, optimized, data, path;
    data = args.data;
    path = args.path;

    try {
      if (this.options.ignored && this.options.ignored.test(args.path)) {
        // ignored file path: return non minified
        const result = {
          data: data,
          // It seems like brunch passes in a SourceMapGenerator object, not a string
          map: args.map ? args.map.toString() : null
        };
        return Promise.resolve(result);
      }
    } catch (e) {
      return Promise.reject('error checking ignored files to uglify' + e);
    }

    try {
      this.options.inSourceMap = JSON.parse(args.map);
    } catch (_e) {} //eslint-disable-line no-empty

    this.options.outSourceMap = this.options.sourceMaps ?
      path + '.map' : undefined;

    try {
      optimized = uglify.minify(data, this.options);
    } catch (_error) {
      error = 'JS minification failed on ' + path + ': ' + _error;
    } finally {
      if (error) return Promise.reject(error);
      const result = optimized && this.options.sourceMaps ? {
        data: optimized.code,
        map: optimized.map
      } : {data: optimized.code};
      result.data = result.data.replace(/\n\/\/# sourceMappingURL=\S+$/, '');
      return Promise.resolve(result);
    }
  }
}

UglifyJSOptimizer.prototype.brunchPlugin = true;
UglifyJSOptimizer.prototype.type = 'javascript';

module.exports = UglifyJSOptimizer;
