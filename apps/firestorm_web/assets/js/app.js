require('../css/app.scss')
import '../../../../deps/phoenix_html/priv/static/phoenix_html'

// Our components
import Reaction from './components/reaction'
import Times from './components/times'
import AutoExpand from './components/auto_expand'
import Tags from './components/tags'
import Posts from './components/posts'

Times.humanize()
AutoExpand.expand()
Tags.handleEditor()
Reaction.mount()
Posts.decorate()
