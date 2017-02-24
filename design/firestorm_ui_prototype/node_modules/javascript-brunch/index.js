'use strict';

const esprima = require('esprima');

class JavaScriptCompiler {
  constructor(brunchCfg) {
    this.config = brunchCfg && brunchCfg.plugins && brunchCfg.plugins.javascript || {};
    this.validate = this.config.validate;
    if (this.validate == null) this.validate = true;
  }

  compile(params) {
    if (this.validate) {
      try {
        const errors = esprima.parse(params.data, {tolerant: true}).errors.map(error => error.message);
        if (errors.length) return Promise.reject(errors);
      } catch (error) {
        return Promise.reject(error);
      }
    }

    return Promise.resolve(params);
  }
}

JavaScriptCompiler.prototype.brunchPlugin = true;
JavaScriptCompiler.prototype.type = 'javascript';
JavaScriptCompiler.prototype.extension = 'js';

module.exports = JavaScriptCompiler;
