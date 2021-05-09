---
order: 1
---
# Hot Reload

**Hot Reloading** is a feature of Ema's dev server wherein any changes to your Haskell source or data files (such as Markdown files or HTML templates) _propagate instantly_ to the web browser without requiring any manual intervention like a full browser refresh. In practice, this is a such a delightful feature to work with. Imagine changing CSS style of an element, and see it reflect on your site in a split second.

## How Ema implements hot reload

### Websocket

The Ema dev server uses websockets to keep a bi-directional connection open between the web browser and the backend server. When you click on a link or when something changes in the backend, they are communicated via this connection. In a statically generated site, however, no such activity happens - and a link click behaves like a normal link, in that the browser makes a full HTTP request to the linked page.

### DOM patching

When switching to a new route or when receiving the new HTML, Ema uses [morphdom](https://github.com/patrick-steele-idem/morphdom) to _patch_ the existing DOM tree rather than replace it in its entirety. This, in addition to use of websockets, makes it possible to support **instant** hot reload with nary a delay. 

### Haskell reload

Finally, hot reload on _code_ changes are supported via [ghcid](https://github.com/ndmitchell/ghcid). The [template repo](https://github.com/srid/ema-docs)'s `bin/run` script uses ghcid underneath. Any HTML DSL (like blaze-html -- as used by the [Tailwind helper](guide/helpers/tailwind.md)) or CSS DSL automatically gets supported for hot-reload. If you choose to use a file-based HTML template language, you can enable hot-reload on template change using the [FileSystem helper](guide/helpers/filesystem.md).

Note that if your application makes use of threads, it is important to setup cleanup handlers so that `ghcid` doesn't leave [ghost](https://stackoverflow.com/q/24999636/55246) processes behind. Helpers like [`race_`](https://hackage.haskell.org/package/async-2.2.3/docs/Control-Concurrent-Async.html#v:race_) will do this automatically (incidentally it is used by `runEma` for running the user IO action).

### Data reload

For anything outside of the Haskell code, your code becomes responsible for monitoring and updating the model [LVar](concepts/lvar.md). The [filesystem helper](guide/helpers/filesystem.md) already provides utilities to facilitate this for monitoring changes to files and directories.

## Handling errors

If your code throws a Haskell exception, they will be gracefully handled and displayed in the browser, allowing you to recover without breaking hot-reload flow.
