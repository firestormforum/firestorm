require('../css/app.scss')
import '../../../../deps/phoenix_html/priv/static/phoenix_html'
// polyfill es6 promises
require('es6-promise')
// polyfill fetch browser api
require('isomorphic-fetch')

// Our components
import Reaction from './components/reaction'
import Times from './components/times'
import AutoExpand from './components/auto_expand'
import Tags from './components/tags'
import Posts from './components/posts'
import Attachments from './components/attachments'

Times.humanize()
AutoExpand.expand()
Tags.handleEditor()
Reaction.mount()
Posts.decorate()
Posts.preview()
Attachments.mount()
