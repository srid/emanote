---
pandoc:
  filters:
    render:
      html:
        - lua-filters/diagram.lua
---

# Diagram filter — d2

```d2
direction: down

root: "where I started"
b1: "side branch"
b2: "another side branch"

root -> b1: meander
b1 -> b2: meander
b2 -> root: "notice & snap back"
```
