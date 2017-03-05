require('../css/app.scss')
import '../../../../deps/phoenix_html/priv/static/phoenix_html'
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
