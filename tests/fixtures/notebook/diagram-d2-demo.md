---
pandoc:
  filters:
    render:
      html:
        - lua-filters/diagram.lua
---

# Diagram filter — d2

```d2
grid-rows: 2

was: "Who I was"
becoming: "Who I was becoming"
what: "What I am"
who: "Who I am"

was -> what
becoming -> who
```
