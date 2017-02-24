require('../css/app.scss')
import 'phoenix_html'
let moment = require('moment')
let $ = require('jquery')

$('abbr.time').html((_, html) => {
  return moment(html).fromNow()
})
