---
pandoc:
  filters:
    render:
      html:
        - lua-filters/hello.lua
---

# Hello Filter Hot Reload

A simple prose paragraph: HELLO_PROSE_TOKEN.

```hello
HELLO_FENCE_TOKEN
```
