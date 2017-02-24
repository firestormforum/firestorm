'use strict';
const _colors = require('ansicolors');
const colorAbility = require('./color-ability');

let colors;

if (colorAbility) {
  colors = _colors;
} else {
  colors = Object.keys(_colors).reduce((acc, color) => {
    acc[color] = x => x;
    return acc;
  }, {});
}

module.exports = colors;
