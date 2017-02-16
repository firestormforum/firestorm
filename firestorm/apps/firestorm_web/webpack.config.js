const webpack = require('webpack')
const path = require('path')

const nodeEnv = process.env.NODE_ENV || 'development'
const isProd = nodeEnv === 'production'

const staticDir = path.join(__dirname, 'web/static')
const jsDir = path.join(staticDir, 'js')
const destDir = path.join(__dirname, 'priv/static')
const publicPath = 'http://localhost:4001/'

module.exports = {
  context: jsDir,
  entry: {
    js: [
      './app',
    ],
  },
  output: {
    path: destDir,
    filename: 'js/app.js',
    publicPath,
  },
  devServer: {
    contentBase: staticDir,
  },
}
