---
order: 2
---
# LVar

If you are familiar with Haskell's `stm` package, a `LVar` is essentially a [`TMVar`](https://hackage.haskell.org/package/stm-2.5.0.0/docs/Control-Concurrent-STM-TMVar.html) but with an extra ability for other threads to observe changes. Ema uses it for [hot reload](concepts/hot-reload.md), and your application code is expected to set and update its [model](guide/model.md) through the LVar.

Documentation on `LVar` is available [on Hackage](https://hackage.haskell.org/package/lvar).