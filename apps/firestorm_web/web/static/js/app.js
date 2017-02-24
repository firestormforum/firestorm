require('../css/app.scss')
let moment = require('moment')
let $ = require('jquery')

$('abbr.time').html((_, html) => {
  return moment(html).fromNow()
})
