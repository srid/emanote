# Tutorial

Make sure that you have have followed [the previous section](start.md) in order to have the [template repo](https://github.com/srid/ema-docs) checked out and running locally. Here, **our goal** is to replace the source code of the template repo and write a basic site from scratch.

1. Follow the template repo's [README](https://github.com/srid/ema-docs#getting-started) and have it open in Visual Studio Code while running the dev server. Your website should be viewable at <http://localhost:9001/>
1. Open `src/Main.hs`
1. Delete everything in it, and replace it with the following

```haskell
module Main where

import qualified Ema

main :: IO ()
main = do
  let speaker :: Text = "Ema"
  Ema.runEmaPure $ \_ ->
    encodeUtf8 $ "<b>Hello</b>, from " <> speaker
```

This is the *minimum* amount of code necessary to run an Ema site. Notice that as you replace and save this file, your browser (which is at <http://locahost:9001>) will [hot reload](concepts/hot-reload.md) to display "Hello, Ema". Congratulations, you just created your first website! 

## Expanding on Hello World

Okay, but that's just *one* page. But we want to add a second page. And might as well add more content than "Hello, Ema". Let's do that next. The first step is define the [route](guide/routes.md) type that corresponds to our site's pages. Add the following:

```haskell
data Route
  = Index  -- Corresponds to /
  | About  -- Corresponds to /about
  deriving (Bounded, Enum, Show)
```

Next, let's define a [model](guide/model.md). A model will hold the state of our website used to render its HTML. Let's put the `speaker` variable in it, as that's all we are using:

```haskell
data Model = Model { speaker :: Text }
```

We should now tell Ema how to convert our `Route` to actual URL paths. Let's do that by making an instance of the `Ema` [class](guide/class.md):

```haskell
import Ema (Ema (..))

instance Ema Model Route where
  encodeRoute = \case
    Index -> []         -- To /
    About -> ["about"]  -- To /about
  decodeRoute = \case
    [] -> Just Index         -- From /
    ["about"] -> Just About  -- From /about
    _ -> Nothing             -- Everything else, are bad routes
```

Now, we write the `main` entry point:

```haskell
import Control.Concurrent (threadDelay)
import qualified Data.LVar as LVar

main :: IO ()
main = do
  Ema.runEma render $ \model -> do
    LVar.set model $ Model "Ema"
    liftIO $ threadDelay maxBound
```

The `runEma` function is explained [here](guide/class.md), but in brief: it takes a render function (see below) as well as an IO action that allows us to create and update the model [lvar](concepts/lvar.md). Note that `threadDelay maxBound` here? That is because our IO action must not exit; in the dev server mode of real-world websites, you would continue to monitor the external world (such as Markdown files) and update the model, to facilitate [hot reload](concepts/hot-reload.md) of data used by your site.

On final piece of the puzzle is to write the aforementioned `render` function:

```haskell
import qualified Ema.CLI
import qualified Ema.Helper.Tailwind as Tailwind
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as RU

render :: Ema.CLI.Action -> Model -> Route -> LByteString
render _emaAction model r = RU.renderHtml $
  H.html $ do
    H.head $ do 
      H.title "Basic site"
    H.body $ do
      H.div ! A.class_ "container" $ do
        case r of
          Index -> do
            H.toHtml $
              "You are on the index page. The name is " <> speaker model
            routeElem About "Go to About"
          About -> do
            "You are on the about page. "
            routeElem Index "Go to Index"
  where
    routeElem targetRoute w =
      H.a 
        ! A.style "text-decoration: underline" 
        ! A.href (H.toValue $ Ema.routeUrl targetRoute) $ w
```

If everything compiles, you should see the site update in the web browser. A couple of quick points about the `render` function:

1. It should return the raw HTML as a `ByteString`. Here, we use [blaze-html](https://hackage.haskell.org/package/blaze-html) as HTML DSL. You can also use your own HTML templates of course.
1. It uses `Ema.routeUrl` function to create a URL out of our `Route` type. This function uses the [`Ema` typeclass](guide/class.md), so it uses the `encodeRoute` function defined further above.

On final note, you will note that nothing is actually *generated* so far. This is because Ema has been running in the dev server mode, which is quite useful during development. To actually generate the files, you can use the `gen` command when running the [CLI](concepts/cli.md):

```bash
mkdir ./output
nix run . -- -C ./content gen ./output
```

## Exercises

1. Figure out how to use static assets (images, files) in your static sites (hint: the typeclass)
2. What happens if you `throw` an exception or use `error` in the `render` function?

{.last}
[Next]{.next}, checkout the [Guide](guide.md) series for information on specific topics.
