# Brunch Skeletons

This is a registry of brunch skeletons. Human-readable version: http://brunch.io/skeletons

## Adding your skeleton

Simply edit `skeletons.json` file and add a new entry *to the top of the file, but below official Brunch skeletons*:

```json
{
  "title": "Brunch with Exim",
  "url": "hellyeahllc/with-exim",
  "alias": "exim",
  "technologies": "Babel, ES6, React, Exim",
  "description": "Very useful for Cordova apps. A simple skeleton that uses HTML5 boilerplate, React and Exim framework."
}
```

* **Title** - Simple and concise title of your skeleton
* **URL** - Git URL. For GitHub `github.com/a/b` may simply be `a/b`.
* **Alias** - a short alias, so users would be able to use `brunch new -s alias` instead of specifying full Git URL.
