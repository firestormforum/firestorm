const $ = require("jquery");

import Prism from "prismjs";
import "prismjs/themes/prism-solarizedlight.css";
import "prismjs/components/prism-elixir";
import "prismjs/components/prism-erlang";
import "prismjs/components/prism-haml";
import "prismjs/components/prism-css";
import "prismjs/components/prism-scss";
import "prismjs/plugins/autolinker/prism-autolinker";
import "prismjs/plugins/autolinker/prism-autolinker.css";
import "prismjs/plugins/line-numbers/prism-line-numbers";
import "prismjs/plugins/line-numbers/prism-line-numbers.css";
import "prismjs/plugins/normalize-whitespace/prism-normalize-whitespace";
import "prismjs/plugins/toolbar/prism-toolbar";
import "prismjs/plugins/toolbar/prism-toolbar.css";

function decorate() {
  Prism.highlightAll();
}

function preview() {
  console.log("TODO: Implement markdown preview!");
}

const Posts = {
  decorate,
  preview
};

export default Posts;
