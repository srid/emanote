---
order: 4
---
# CLI

Ema apps have a basic CLI argument structure that takes two kinds of input:

1. `-C <dir>`: specifies the "input directory" (current working directory by default)
2. `gen` subcommand: generates the static site, instead of starting up the dev server

Ema (`runEma`) will change the [current working directory](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html#v:getCurrentDirectory) to the "input directory" before running your application code. It, along with the "gen" subcommand (if used), is passed as the `Ema.CLI.Action` type to your `render` function. You can also use `runEmaWith` if you are manually handling the CLI arguments yourself.
