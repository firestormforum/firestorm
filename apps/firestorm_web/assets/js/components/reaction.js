const $ = require('jquery')
const elmEmojiPicker = require('../vendor/elm-emoji-picker')

const createWidget = ($el) => {
  let widget = $("<div>").addClass("widget")[0]
  let $form = $("form", $el)
  let $emojiInput = $("input.emoji", $form)
  $el.append(widget)
  let app = elmEmojiPicker.Main.embed(widget)
  app.ports.selectedEmoji.subscribe((selected) => {
    $emojiInput.val(selected)
    $form.submit()
  })
  let close = () => {
    $(widget).remove()
    $($el).one("click", function() { createWidget(this) })
  }
  setTimeout(() => {
    $(document).one("click", close)
    $(widget).click((e) => e.stopPropagation())
    let $widgetInput = $("input", widget)
    $widgetInput.focus()
  }, 0)
}

const mount = () => {
  $('.reaction-picker form').hide()
  $('.reaction-picker').one("click", function() { createWidget(this) })
}

const Reaction = {
  mount: mount
}

export default Reaction
