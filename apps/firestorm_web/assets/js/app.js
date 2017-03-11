require('../css/app.scss')
import '../../../../deps/phoenix_html/priv/static/phoenix_html'
import Prism from 'prismjs'
import 'prismjs/themes/prism-solarizedlight.css'
import 'prismjs/components/prism-elixir'
import 'prismjs/components/prism-erlang'
import 'prismjs/components/prism-haml'
import 'prismjs/components/prism-css'
import 'prismjs/components/prism-scss'
import 'prismjs/plugins/autolinker/prism-autolinker'
import 'prismjs/plugins/autolinker/prism-autolinker.css'
import 'prismjs/plugins/line-numbers/prism-line-numbers'
import 'prismjs/plugins/line-numbers/prism-line-numbers.css'
import 'prismjs/plugins/normalize-whitespace/prism-normalize-whitespace'
import 'prismjs/plugins/toolbar/prism-toolbar'
import 'prismjs/plugins/toolbar/prism-toolbar.css'

let moment = require('moment')
let $ = require('jquery')

$('abbr.time').html((_, html) => {
  return moment.utc(html.trim(), moment.ISO_8601).fromNow()
})

// Handle textarea autoexpand
// http://codepen.io/vsync/pen/frudD
// Applied globally on all textareas with the "autoexpand" class
$(document)
  .one('focus.autoexpand', 'textarea.autoexpand', function() {
    let savedValue = this.value
    this.value = ''
    this.baseScrollHeight = this.scrollHeight
    this.value = savedValue
  })
  .on('input.autoexpand', 'textarea.autoexpand', function() {
    let minRows = this.getAttribute('data-min-rows')|0, rows
    let rowHeight = 26 // This is a magic number based on the font size, sorry
    this.rows = minRows
    rows = Math.ceil((this.scrollHeight - this.baseScrollHeight) / rowHeight)
    this.rows = minRows + rows
  })

Prism.highlightAll()
