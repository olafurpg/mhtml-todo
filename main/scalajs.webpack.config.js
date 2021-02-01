module.exports = {
  "entry": {
    "mhtml-todo-opt": "/Users/olafurpg/dev/olafurpg/mhtml-todo/target/scala-2.12/scalajs-bundler/main/opt-launcher.js"
  },
  "output": {
    "path": "/Users/olafurpg/dev/olafurpg/mhtml-todo/target/scala-2.12/scalajs-bundler/main",
    "filename": "[name]-bundle.js"
  },
  "devtool": "source-map",
  "module": {
    "preLoaders": [{
      "test": new RegExp("\\.js$"),
      "loader": "source-map-loader"
    }]
  }
}