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

bad: Feeling bad
good: Feeling good
happy: Happy & harmless
excellent: Feeling excellent

bad -> good
good -> happy
happy -> excellent
```
