require("../css/app.scss");
import "../../deps/phoenix_html/priv/static/phoenix_html";

// == POLYFILLS ==
// polyfill es6 promises
require("es6-promise");
// polyfill fetch browser api
require("isomorphic-fetch");
// == END POLYFILLS ==

// == COMPONENTS ==
import Posts from "./components/posts";
// == END COMPONENTS ==

// == USING COMPONENTS ==
// ==== POSTS ====
// Decorate posts
Posts.decorate();
// Preview markdown
Posts.preview();
// ==== END POSTS ====
// == END USING COMPONENTS ==
