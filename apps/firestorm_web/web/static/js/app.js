require('../css/app.scss')
import '../../../../../deps/phoenix_html/priv/static/phoenix_html'
let moment = require('moment')
let $ = require('jquery')

$('abbr.time').html((_, html) => {
  return moment(html).fromNow()
})
