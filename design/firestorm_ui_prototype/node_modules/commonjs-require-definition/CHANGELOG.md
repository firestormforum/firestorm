# commonjs-require-definition 0.4.1 (Mar 7. 2016)
* `require.reset()` to reset all registered modules

# commonjs-require-definition 0.4.0 (Mar 4, 2016)
* Handle registers with file extensions, create extension-less aliases.
  For example, registering `app/something.js` will also create an alias `app/something`.
  In case of multiple extensions, several aliases will be created — `app/menu.tmpl.hbs` will create aliases `app/menu.tmpl` and `app/menu`.

# commonjs-require-definition 0.3.0 (Feb 22, 2015)
* Removed component(1) support.

# commonjs-require-definition 0.2.2 (27 Oct 2015)
* Small fix for mocking.

# commonjs-require-definition 0.2.1 (18 Oct 2015)
* Now exposing `require._cache` for testing / mocking purposes.

# commonjs-require-definition 0.2.0 (7 April 2015)
* Added component(1) support.
* Improved minifier compatibility.

# commonjs-require-definition 0.1.2 (17 October 2013)
* Add support for dependency cycles.

# commonjs-require-definition 0.1.1 (7 August 2013)
* Add better msgs: Cannot file module "views/user-view" from "controllers/user"

# commonjs-require-definition 0.1.0 (20 July 2013)
* Initial release.
