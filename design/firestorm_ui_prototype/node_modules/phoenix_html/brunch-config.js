exports.config = {
  sourceMaps: false,
  production: true,

  modules: {
    wrapper: false,
    definition: false
  },

  npm: {
    enabled: false
  },

  files: {
    javascripts: {
      joinTo: 'phoenix_html.js'
    },
  },

  paths: {
    // Which directories to watch
    watched: ["web/static", "test/static"],

    // Where to compile files to
    public: "priv/static"
  },

  plugins: {
    babel: {
      // Do not use ES6 compiler in vendor code
      ignore: [/^(web\/static\/vendor)/]
    }
  }
};
