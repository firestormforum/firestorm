require('./main.css')
let logoPath = require('./logo.svg')
let Elm = require('./Main.elm')

let root = document.getElementById('root')

Elm.Main.embed(root, null)
