module.exports = function(callback) { return function() {
  'use strict'; // MIT License, paulmillr.com
  if (typeof callback !== 'function') throw new TypeError('promisify must receive a function');
  for (var args = new Array(arguments.length), i = 0; i < args.length; ++i) args[i] = arguments[i]; // git.io/vk55A
  var _this = this;
  return new Promise(function(resolve, reject) {
    args.push(function(error, data) {error == null ? resolve(data) : reject(error)});
    callback.apply(_this, args);
  });
}};
