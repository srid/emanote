---
order: 1
---
# Defining your model

A "*model*" in Ema represents the state to use to generate your site. It could be as simple as a variable, or it could be a list of parsed Markdown files (as in the case of a weblog). Ema's model is also conceptually similar to [Elm](https://guide.elm-lang.org/architecture/)'s model, in that - changing the model [automatically](concepts/hot-reload.md) changes the [view](guide/render.md).

Here's an example model:

```haskell
newtype BlogPosts = BlogPosts (Map Slug Text}
```

Here `BlogPosts` is the model type. If we are generating a weblog site, then all the "data" we need is loaded into memory as a value of `BlogPosts`.

## Modifying the model

Ema's dev server supports [hot reload](concepts/hot-reload.md); it will observe changes to your model, in addition to code. To facilitate this you will manage your model as a [LVar](concepts/lvar.md). The `runEma` function ([described here](guide/class.md)) takes an IO action that gets `LVar model` as an argument. 

For example,

```haskell
runEma render $ \model ->
  forever $ do
    LVar.set model =<< liftIO getCurrentTime
    liftIO $ threadDelay $ 1 * 1000000
```

In this contrived example ([full code here](https://github.com/srid/ema/blob/master/src/Ema/Example/Ex02_Clock.hs)), we are using `UTCTime` as the model. We set the initial value using `LVar.set`, and then continually update the current time every second. Every time the model gets updated, the web browser will [hot reload](concepts/hot-reload.md) to display the up to date value. For the `BlogPosts` model, you would typically use [fsnotify](https://hackage.haskell.org/package/fsnotify) to monitor changes to the underlying Markdown files, but note that Ema provides [a helper](guide/helpers/filesystem.md) for that.


{.last}
[Next]{.next}, we will [talk about routes](guide/routes.md).