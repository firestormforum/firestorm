const moment = require("moment");
const $ = require("jquery");

const humanize = () => {
  $("abbr.time").html((_, html) => {
    return moment.utc(html.trim(), moment.ISO_8601).fromNow();
  });
};

const Times = {
  humanize
};

export default Times;
