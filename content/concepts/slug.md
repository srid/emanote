---
order: 3
---
# Slug

A slug is a component of a URL or file path. In an _URL_ like `/foo/bar`, there are two _slugs_: "foo" and "bar". URLs (as well as filepaths) can therefore be represented as lists of slugs (`[Slug]`).

```haskell
import Ema (Slug)

type URL = [Slug]
```

Slugs are integral to Ema's routing system. When defining [route](guide/routes.md) encoders and decoders (via [Ema class instance](guide/class.md)) you are effectively writing functions that convert back and forth between your route type and `[Slug]`. These functions are ultimately used to determine the *filename* of the statically generated HTML (i.e., `./foo/bar.html`) as well as the linking URL in the rendered HTML (i.e., `/foo/bar`).

Slugs are also automatically [unicode normalized](https://www.unicode.org/faq/normalization.html) to NFC to ensure that route links work reliably regardless of the underlying representation of any non-ascii link text.