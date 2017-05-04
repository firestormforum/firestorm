const $ = require("jquery");

function scrollToFirstUnreadPost() {
  const $scrollArea = $(".pure-pusher");
  const $firstUnreadPost = $(".post-item.first-unread");

  if ($firstUnreadPost[0]) {
    $scrollArea.scrollTop($firstUnreadPost.offset().top);
  }
}

const Threads = {
  behave: () => {
    scrollToFirstUnreadPost();
  }
};

export default Threads;
