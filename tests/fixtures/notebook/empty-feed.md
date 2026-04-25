---
feed:
  enable: true
---

# Empty feed

A feed-enabled note that contains no `query` code block. Before issue
[#490](https://github.com/srid/emanote/issues/490) this aborted
`emanote gen` with `Bad feed: ... can't find note query` and broke the
live server's atom route for the note. The fix degrades gracefully to
an empty-but-valid Atom feed.
