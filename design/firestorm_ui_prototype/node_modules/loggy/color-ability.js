const colorAbility = (() => {
  if ('FORCE_NO_COLOR' in process.env) {
    return false;
  }

  // adapted from https://github.com/chalk/supports-color/blob/master/index.js
  if (process.stdout && !process.stdout.isTTY) {
    return false;
  }

  if (process.platform === 'win32') {
    return true;
  }

  if ('COLORTERM' in process.env) {
    return true;
  }

  if (process.env.TERM === 'dumb') {
    return false;
  }

  if (/^screen|^xterm|^vt100|color|ansi|cygwin|linux/i.test(process.env.TERM)) {
    return true;
  }

  return false;
})();

module.exports = colorAbility;
