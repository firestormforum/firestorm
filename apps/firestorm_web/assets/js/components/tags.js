const $ = require('jquery')

const handleEditor = () => {
  $('.tag-editor .add-tag').on('click', function() {
    let $parent = $(this).parent()
    $parent.addClass('editing')
    setTimeout(() => {
      $(document).one("click", () => $parent.removeClass('editing') )
    }, 0)
  })
}

const Tags ={
  handleEditor
}

export default Tags
