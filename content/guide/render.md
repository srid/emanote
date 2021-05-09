---
order: 4
---
# Rendering HTML

Once you have [model](guide/model.md) and [routes](guide/routes.md) in place and [constrained](guide/class.md), the last piece of the puzzle is to write a function that takes both as arguments and returns the HTML string (lazy bytestring, to be exact). This function can be as simple as the following:

```haskell
render :: MyModel -> Route -> ByteString
render model route =
  "<b>Hello</b>, world!"
```

Of course we want it to be real, by using our model value, as well as generate the HTML based on the route. We will also use the [blaze-html](https://hackage.haskell.org/package/blaze-html) library to make writing HTML in Haskell palatable (see also [the layout helper](guide/helpers/tailwind.md)). A more realistic starting point (if not the finishing product) would be:

```haskell
render :: MyModel -> Route -> ByteString 
render model route = Blaze.renderHtml $ 
  H.html $ do 
    H.head $ do 
      H.title "My site"
    H.body $ do 
      H.h1 "My site"
      case route of 
        Index -> 
          H.h1 "Welcome to my website!"
          H.p $ do 
            "Checkout the"
            H.a ! A.href (H.toValue $ Ema.routeUrl About) $ "About"
            " page."
        About ->
          H.div $ H.p "This website is managed by yours truly"
      H.footer $ do 
        A.a ! A.href "https://github.com/user/repo" $ "Source on GitHub"
```

Note that Ema provides a `routeUrl` helper function that serializes your route to the final URL (here, `/about`) for linking to.

Spend a few moments trying to appreciate how this is *much simpler* to write than dealing with HTML template files spread across the disk as is the case with traditional static site generators. If you [choose](https://vrom911.github.io/blog/html-libraries) to go the DSL route, Haskell's type-safety now applies to your HTML as well. On top of it, Ema's [hot reload](concepts/hot-reload.md) will instantly update the dev server's browser view whenever you change your HTML (or any of the Haskell source code).

Of course when using Ema nothing prevents you from choosing to use traditional HTML templates, and you can get [hot reload](concepts/hot-reload.md) on them too with [a little bit of plumbing](guide/helpers/filesystem.md).

{.last}
[Next]{.next}, you might want to peruse [the helper topics](guide/helpers.md) if you need some extra functionality provided.
