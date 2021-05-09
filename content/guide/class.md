---
order: 3
---
# Defining Ema instance

Once you have [model](guide/model.md) and [route](guide/routes.md) types in place, we must tell the Haskell compiler that they are suitable for generating static sites. We do this by creating an instance of the `Ema` typeclass.

Using some `MyModel` and the route `Route` shown in the [previous](guide/routes.md) section, we can create an instance as follows:

```haskell
class Ema MyModel Route where 
  -- Convert our route to browser URL, represented as a list of slugs
  encodeRoute = \case
    Index -> []  -- An empty slug represents the index route: index.html
    About -> ["about"]

  -- Convert back the browser URL, represented as a list of slugs, to our route
  decodeRoute = \case
    [] -> Just Index
    ["about"] -> Just About
    _ -> Nothing

  -- The third method is optional; and used during static site generation. 
  -- This tells Ema which routes to generate .html files for.
  -- By default, Enum & Bounded will be used to determine this list.
  staticRoutes model =
    [Index, About]

  -- The fourth method is also optional; if you have static assets to serve, specify
  -- them here. Paths are relative to current working directory.
  staticAssets Proxy =
    ["css", "images", "favicon.ico", "resume.pdf"]
```

The `Ema` typeclass has four methods, with the last two of them being optional with default implementations:

1. Define `encodeRoute` that converts our route type to a browser URL [slug](concepts/slug.md) path representing relative URLs like `/foo/bar`
2. Define `decodeRoute` that does the *reverse* converstion (the conversion must be isomorphic)
3. _Optionally_, define `staticRoutes` indicating the routes to statically generate
4. _Optionally_, define the list of static assets to copy over as-is during static generation

## `runEma`

The `Ema` constraint is used by the `runEma` function that acts as the main entry point to your static site generator. It takes two arguments:

1. `render` function that renders your HTML (we'll go over this in [the next](guide/render.md) section) 
2. an IO action that takes [`LVar model`](guide/model.md) as an argument. 
 
This IO action is expected to be a long-running one, wherein you have full control over setting the value of the model over time.

{.last}
[Next]{.next}, with our model and routes in place constrained by `Ema` type class, [we will define the HTML for our site](guide/render.md) using Ema.
