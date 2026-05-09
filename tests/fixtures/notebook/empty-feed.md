---
feed:
  enable: true
---

# Empty feed

A feed-enabled note whose query matches no notes. Before issue
[#490](https://github.com/srid/emanote/issues/490) this aborted
`emanote gen` with `Bad feed: ... no notes matched the query` and broke
the live server's atom route. The fix degrades the empty-result case
to a valid empty Atom feed; configuration errors (missing `query`
block, invalid query) still fail loud.

```query
path:./does-not-exist/*
```
