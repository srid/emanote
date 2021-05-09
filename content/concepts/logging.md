---
order: 5
---
# Logging

`runEma`'s action monad supports the `MonadLoggerIO` constraint, as defined by [monad-logger](https://hackage.haskell.org/package/monad-logger). This means that you can use any of the logging functions from `monad-logger` to add logging to your application. [monad-logger-extras](https://hackage.haskell.org/package/monad-logger-extras) is used to colorize the logs.

```haskell
logInfoNS "myapp" "This is an info message"
logDebugNS "myapp" "This is a debug message info"