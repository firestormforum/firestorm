const $ = require('jquery')

const expand = () => {
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
}

const AutoExpand = {
  expand
}

export default AutoExpand
