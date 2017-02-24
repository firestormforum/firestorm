'use strict';

const cp = require('child_process'), spawn = cp.spawn, exec = cp.exec;
const sysPath = require('path');
const progeny = require('progeny');
const libsass = require('node-sass');
const os = require('os');
const anymatch = require('anymatch');

const postcss = require('postcss');
const postcssModules = require('postcss-modules');

const cssModulify = (path, data, map, options) => {
  let json = {};
  const getJSON = (_, _json) => json = _json;

  return postcss([postcssModules(Object.assign({}, {getJSON}, options))])
    .process(data, {from: path, map}).then(x => {
      const exports = 'module.exports = ' + JSON.stringify(json) + ';';
      return { data: x.css, map: x.map, exports };
    });
};

const isWindows = os.platform() === 'win32';
const compassRe = /compass/;
const sassRe = /\.sass$/;

const promisify = fn => function() {
  const args = [].slice.call(arguments);
  return new Promise((resolve, reject) => {
    args.push((error, data) => {
      if (error != null) return reject(error);
      resolve(data);
    });
    fn.apply(this, args);
  });
};

const promiseSpawnAndPipe = (cmd, args, env, data) => {
  let result = '';
  let error;

  return new Promise((resolve, reject) => {
    var sass = spawn(cmd, args, env);
    sass.stdout.on('data', buffer => {
      result += buffer.toString();
    });
    sass.stderr.on('data', buffer => {
      if (error == null) error = '';
      error += buffer.toString();
    });
    sass.on('close', () => {
      if (error) return reject(error);
      resolve(result);
    });
    if (sass.stdin.write(data)) {
      sass.stdin.end();
    } else {
      sass.stdin.on('drain', () => {
        sass.stdin.end();
      });
    }
  });
};

class SassCompiler {
  constructor(cfg) {
    if (cfg == null) cfg = {};
    this.rootPath = cfg.paths.root;
    this.optimize = cfg.optimize;
    this.config = (cfg.plugins && cfg.plugins.sass) || {};
    this.modules = this.config.modules || this.config.cssModules;

    if (this.modules && this.modules.ignore) {
      this.isIgnored = anymatch(this.modules.ignore);
      delete this.modules.ignore;
    } else {
      this.isIgnored = anymatch([]);
    }

    delete this.config.modules;
    delete this.config.cssModules;
    this.mode = this.config.mode;
    if (this.config.options != null && this.config.options.includePaths != null) {
      this.includePaths = this.config.options.includePaths;
    }
    this.getDependencies = progeny({
      rootPath: this.rootPath,
      altPaths: this.includePaths,
      reverseArgs: true
    });
    this.seekCompass = promisify(progeny({
      rootPath: this.rootPath,
      exclusion: '',
      potentialDeps: true
    }));
    /*eslint-disable camelcase */
    this.gem_home = this.config.gem_home;
    this.env = {};
    if (this.gem_home) {
      const env = Object.assign({}, process.env);
      env.GEM_HOME = this.gem_home;
      this.env = {
        env: env
      };
      this._bin = this.gem_home + '/bin/' + this._bin;
      this._compass_bin = this.gem_home + '/bin/' + this._compass_bin;
    }
    /*eslint-enable camelcase */
    this.bundler = this.config.useBundler;
    this.prefix = this.bundler ? 'bundle exec ' : '';
  }

  _checkRuby() {
    const prefix = this.prefix;
    const env = this.env;
    const sassCmd = prefix + this._bin + ' --version';
    const compassCmd = prefix + this._compass_bin + ' --version';

    const sassPromise = new Promise((resolve, reject) => {
      exec(sassCmd, env, error => {
        if (error) {
          console.error('You need to have Sass on your system');
          console.error('Execute `gem install sass`');
          reject();
        } else {
          resolve();
        }
      });
    });
    const compassPromise = new Promise(resolve => {
      exec(compassCmd, env, error => {
        this.compass = !error;
        resolve();
      });
    });
    this.rubyPromise = Promise.all([sassPromise, compassPromise]);
  }

  _getIncludePaths(path) {
    let includePaths = [this.rootPath, sysPath.dirname(path)];
    if (Array.isArray(this.includePaths)) {
      includePaths = includePaths.concat(this.includePaths);
    }
    return includePaths;
  }

  _nativeCompile(source) {
    return new Promise((resolve, reject) => {
      var debugMode = this.config.debug;
      var hasComments = debugMode === 'comments' && !this.optimize;

      libsass.render({
        file: source.path,
        data: source.data,
        precision: this.config.precision,
        includePaths: this._getIncludePaths(source.path),
        outputStyle: (this.optimize ? 'compressed' : 'nested'),
        sourceComments: hasComments,
        indentedSyntax: sassRe.test(source.path),
        outFile: 'a.css',
        functions: this.config.functions,
        sourceMap: true,
        sourceMapEmbed: !this.optimize && this.config.sourceMapEmbed
      },
      (error, result) => {
        if (error) {
          // libsass provides a neat error message, but it's set as `error.formatted`
          // instead of `error.message`. We'll create a new error with `.message` set to
          // the original's `.formatted`.
          reject(new Error(error.formatted));
        } else {
          const css = result.css.toString().replace('/*# sourceMappingURL=a.css.map */', '');
          const map = JSON.parse(result.map.toString());
          resolve({data: css, map: map});
        }
      });
    });
  }

  _rubyCompile(source) {
    if (this.rubyPromise == null) this._checkRuby();
    let cmd = [
      this._bin,
      '--stdin'
    ];

    const includePaths = this._getIncludePaths(source.path);
    includePaths.forEach(path => {
      cmd.push('--load-path');
      cmd.push(path);
    });
    if (!this.config.allowCache) cmd.push('--no-cache');

    if (this.bundler) cmd.unshift('bundle', 'exec');

    return this.rubyPromise.then(() => {
      var debugMode = this.config.debug, hasComments;
      if ((debugMode === 'comments' || debugMode === 'debug') && !this.optimize) {
        hasComments = this.config.debug === 'comments';
        cmd.push(hasComments ? '--line-comments' : '--debug-info');
      }

      if (this.config.precision) cmd.push('--precision=' + this.config.precision);
      if (!sassRe.test(source.path)) cmd.push('--scss');
      if (source.compass && this.compass) cmd.push('--compass');
      if (this.config.options != null) cmd.push.apply(cmd, this.config.options);

      if (isWindows) {
        cmd = ['cmd', '/c', '"' + cmd[0] + '"'].concat(cmd.slice(1));
        this.env.windowsVerbatimArguments = true;
      }

      return promiseSpawnAndPipe(cmd[0], cmd.slice(1), this.env, source.data).then(d => { return {data: d}; });
    });
  }

  compile(params) {
    const data = params.data;
    const path = params.path;

    // skip empty source files
    if (!data.trim().length) return Promise.resolve({data: ''});

    return this.seekCompass(path, data).then(imports => {
      const source = {
        data: data,
        path: path,
        compass: imports.some(depPath => compassRe.test(depPath))
      };

      const fileUsesRuby = sassRe.test(path) || source.compass;

      if (this.mode === 'ruby' || (this.mode !== 'native' && fileUsesRuby)) {
        return this._rubyCompile(source);
      } else {
        return this._nativeCompile(source);
      }
    }).then(params => {
      if (this.modules && !this.isIgnored(path)) {
        const moduleOptions = this.modules === true ? {} : this.modules;
        return cssModulify(path, params.data, params.map, moduleOptions);
      } else {
        return params;
      }
    });
  }
}

SassCompiler.prototype.brunchPlugin = true;
SassCompiler.prototype.type = 'stylesheet';
SassCompiler.prototype.pattern = /\.s[ac]ss$/;
SassCompiler.prototype._bin = isWindows ? 'sass.bat' : 'sass';
SassCompiler.prototype._compass_bin = isWindows ? 'compass.bat' : 'compass'; //eslint-disable-line camelcase

module.exports = SassCompiler;
