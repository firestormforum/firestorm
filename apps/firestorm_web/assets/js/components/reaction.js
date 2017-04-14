const elmEmojiPicker = require('../vendor/elm-emoji-picker')
const $ = require('jquery')

const createWidget = ($el) => {
  let widget = $("<div>").addClass("widget -hidden")[0]
  let $form = $("form", $el)
  let $emojiInput = $("input.emoji", $form)
  $el.append(widget)
  const app = elmEmojiPicker.Main.embed(widget)
  app.ports.selectedEmoji.subscribe((selected) => {
    $emojiInput.val(selected)
    $form.submit()
  })
}

const mount = () => {
  $('.reaction-picker form').hide()
  $('.reaction-picker').one("click", function() { createWidget(this) })
}

const Reaction = {
  mount: mount
}

export default Reaction
